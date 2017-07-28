(ns fulcro-graphql.main
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [<! >! put! promise-chan]]
            [cljs.spec.alpha :as s]
            [common.async :refer-macros [<? go-catch]]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [fulcro.client.network :as fulcro.network]
            [goog.events :as events]
            [goog.string :as gstr]
            [om.dom :as dom]
            [om.next :as om]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.graphql :as gql]
            [spec-coerce.core :as sc])
  (:import [goog.net XhrIo EventType]))

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defn js-name [s]
  (gstr/toCamelCase (name s)))

(def parser (om/parser {:read p/pathom-read}))

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
    (events/listen xhr (.-SUCCESS EventType) #(put! c [[% (.getResponseText xhr)] nil]))
    (events/listen xhr (.-ERROR EventType) #(put! c [nil %]))
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

(defrecord Network [url completed-app]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [_ edn ok error]
    (go
      (try
        (-> (query #::{:url url :q edn})
            <? ok)
        (catch :default e (error e)))))

  (start [this app] (assoc this :complete-app app)))

(defn make-network [url]
  (map->Network {:url url}))

(defonce app
  (atom (fulcro/new-fulcro-client :networking (make-network "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"))))

(s/def :lifecycle/created-at inst?)

(om/defui ^:once UiLink
  static om/IQuery
  (query [_] [:link/id :link/description :lifecycle/created-at :link/url])

  static om/Ident
  (ident [_ props] [:link/by-id (:link/id props)])

  Object
  (render [this]
    (let [{:link/keys [description]
           :keys      [lifecycle/created-at] :as props} (om/props this)]
      (dom/div nil
        (str created-at) " - " description))))

(def ui-link (om/factory UiLink {:keyfn :link/id}))

(om/defui ^:once Root
  static om/IQuery
  (query [_] [{:link/all-links (get-load-query UiLink)}])

  Object
  (render [this]
    (let [{:keys [link/all-links]} (om/props this)]
      (dom/div nil
        (dom/button #js {:onClick #(fetch/load this :link/all-links UiLink)}
          "Load Links")
        (if (fetch/loading? (:ui/fetch-state all-links))
          (dom/div nil "Loading..."))
        (if (sequential? all-links)
          (map ui-link all-links))))))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(init)

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(s/def :link/updated-at inst?)

(comment
  (go
    (->> (query #::{:url "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                    :q   [{:link/all-links [:link/id :link/description :link/url :link/updated-at]}]})
         <? js/console.log))

  (println (gql/query->graphql [{:link/all-links [:link/id :link/description :link/url :link/updated-at]}]
                               {::gql/js-name js-name}))

  (println (gql/query->graphql `[{:viewer
                                  [:login
                                   ({:repositories
                                     [{:nodes [:name :description]}]}
                                     {:last 10})]}
                                 ({:repository [:id :description]}
                                   {:name "spec-coerce" :owner "wilkerlucio"})])))
