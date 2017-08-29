(ns com.wsscode.fulcro-graphql.network
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! put! promise-chan]]
            [com.wsscode.common.async :refer-macros [<? go-catch]]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.graphql :as gql]
            [fulcro.client.network :as fulcro.network]
            [goog.events :as events]
            [goog.object :as gobj]
            [goog.string :as gstr]
            [om.next :as om]
            [spec-coerce.core :as sc])
  (:import [goog.net XhrIo EventType]))

(defn js-name [s]
  (gstr/toCamelCase (name s)))

(defn mutation [{::p/keys [entity js-key-transform]} key params]
  {:action
   (fn []
     (if-let [[field id] (gql/find-id params)]
       (let [new-id (gobj/getValueByKeys entity #js [(js-key-transform key)
                                                     (js-key-transform field)])]
         {:tempids {id new-id}})
       nil))})

(def parser (om/parser {:read p/pathom-read :mutate mutation}))

(defn parse [env tx]
  (parser
    (merge {::p/js-key-transform   js-name
            ::p/js-value-transform sc/coerce
            ::p/reader             p/js-obj-reader}
           env)
    tx))

(defn http [{::keys [url body method headers]
             :or    {method "GET"}}]
  (let [c   (promise-chan)
        xhr (XhrIo.)]
    (events/listen xhr (.-SUCCESS EventType) #(put! c [% (.getResponseText xhr)]))
    (events/listen xhr (.-ERROR EventType) #(put! c %))
    (.send xhr url method body (clj->js headers))
    c))

(defn lift-tempids [res]
  (->> res
       (into {} (map (fn [[k v]]
                       (if (symbol? k)
                         [k (:result v)]
                         [k v]))))))

(defn query [{::keys [url q] :as input}]
  (go-catch
    (let [[res text] (-> (http #::{:url     url
                                   :method  "post"
                                   :headers {"content-type" "application/json"}
                                   :body    (js/JSON.stringify #js {:query (gql/query->graphql q {::gql/js-name js-name})})})
                         <?)]
      (if (.-error res)
        (throw (ex-info (.-error res) {:query q}))
        (assoc input ::response-data (js/JSON.parse text))))))

(defrecord Network [url]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [this edn ok error]
    (go
      (try
        (let [json (-> (query #::{:url url :q edn}) <? ::response-data)]
          (ok (-> (parse {::p/entity (.-data json)} edn)
                  (lift-tempids))))

        (catch :default e
          (js/console.log "Network error" e)
          (error e)))))

  (start [_]))

(defn graphql-network [url]
  (map->Network {:url url}))
