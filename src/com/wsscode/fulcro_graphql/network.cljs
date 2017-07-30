(ns com.wsscode.fulcro-graphql.network
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<! >! put! promise-chan]]
            [common.async :refer-macros [<? go-catch]]
            [fulcro.client.network :as fulcro.network]
            [goog.events :as events]
            [goog.string :as gstr]
            [om.next :as om]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.graphql :as gql]
            [spec-coerce.core :as sc])
  (:import [goog.net XhrIo EventType]))

(defn js-name [s]
  (gstr/toCamelCase (name s)))

(def parser (om/parser {:read p/pathom-read :mutate om/dispatch}))

(defn parse [env tx]
  (parser
    (assoc env
      ::p/js-key-transform js-name
      ::p/js-value-transform sc/coerce
      ::p/reader p/js-obj-reader)
    tx))

(defn http [{::keys [url body method headers]
             :or    {method "GET"}}]
  (let [c   (promise-chan)
        xhr (XhrIo.)]
    (events/listen xhr (.-SUCCESS EventType) #(put! c [% (.getResponseText xhr)]))
    (events/listen xhr (.-ERROR EventType) #(put! c %))
    (.send xhr url method body (clj->js headers))
    c))

(defn query [{::keys [url q]}]
  (go-catch
    (let [[res text] (-> (http #::{:url     url
                                   :method  "post"
                                   :headers {"content-type" "application/json"}
                                   :body    (js/JSON.stringify #js {:query (gql/query->graphql q {::gql/js-name js-name})})})
                         <?)]
      (if (.-error res)
        (throw (ex-info (.-error res) {:query q}))
        (parse {::p/entity (.-data (js/JSON.parse text))} q)))))

(defrecord Network [url]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [_ edn ok error]
    (go
      (try
        (-> (query #::{:url url :q edn})
            <? ok)
        (catch :default e
          (js/console.log "ERROR NET" e)
          (error e)))))

  (start [this app]))

(defn graphql-network [url]
  (map->Network {:url url}))

(comment
  (go
    (->> (query #::{:url "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                    :q   [{:link/all-links [:link/id :link/description :link/url :link/updated-at]}]})
         <? js/console.log))

  (go
    (->> (query #::{:url "https://api.github.com/graphql?access_token="
                    :q   `[{:viewer
                            [:login
                             ({:repositories
                               [{:nodes [:name :description]}]}
                               {:last 10})]}
                           ({:repository [:id :description]}
                             {:name "spec-coerce" :owner "wilkerlucio"})]})
         <? js/console.log))

  (println (gql/query->graphql [{:link/all-links [:link/id :link/description :link/url :link/updated-at]}]
                               {::gql/js-name js-name}))

  (js/console.log
    (gql/query->graphql `[{:viewer
                           [:login
                            ({:repositories
                              [{:nodes [:name :description]}]}
                              {:last 10})]}
                          ({:repository [:id :description]}
                            {:name "spec-coerce" :owner "wilkerlucio"})])))
