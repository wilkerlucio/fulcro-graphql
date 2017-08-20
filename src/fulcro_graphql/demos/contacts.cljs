(ns fulcro-graphql.demos.contacts
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.next :as om]
            [om.dom :as dom]
            [goog.object :as gobj]
            [cljs.core.async :refer [<!]]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.async :as pa]
            [com.wsscode.pathom.graphql :as gql]
            [com.wsscode.fulcro-graphql.network :as gn]
            [fulcro-graphql.styles :as style]
            [fulcro-css.css :as css]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [com.wsscode.common.local-storage :as local-storage]
            [fulcro.client.network :as fulcro.network]))

(defn get-token []
  (if-let [token (local-storage/get "github-token")]
    token
    (let [token (js/prompt "Please enter github token:")]
      (local-storage/set! "github-token" token)
      token)))

(om/defui ^:once Contact
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:contact/id :contact/name :contact/github
              {:contact/github-node [:github/avatar-url]}])

  static om/Ident
  (ident [_ props] [:contact/by-id (:contact/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:contact/keys [id name github github-node]} (om/props this)
          css (css/get-classnames Contact)]
      (dom/div nil
        (dom/div nil (dom/img #js {:src (:github/avatar-url github-node)}))
        (dom/div nil id)
        (dom/div nil name)
        (dom/div nil github)
        (dom/hr nil)))))

(def contact (om/factory Contact))

(om/defui ^:once Root
  static fulcro/InitialAppState
  (initial-state [_ _] {:ui/react-key (random-uuid)})

  static om/IQuery
  (query [_] [{:app/all-contacts (om/get-query Contact)}
              :ui/react-key])

  static css/Global
  (global-rules [_] [[:body {:background (style/color-white)}]])

  Object
  (render [this]
    (let [{:keys [app/all-contacts ui/react-key]} (om/props this)]
      (dom/div #js {:key react-key}
        (map contact all-contacts)))))

(declare composed-query)

(defmulti attr-handler p/key-dispatch)

(defmethod attr-handler :default [_] ::p/continue)

(defmethod attr-handler :contact/github-node [{:keys [query ::p/entity] :as env}]
  (go
    (let [github (gobj/get entity "github")
          query  [(list {:github/user query} {:login github})]]
      (-> (composed-query #::{:q            query
                              :url          (str "https://api.github.com/graphql?access_token=" (get-token))
                              :attr-handler attr-handler})
          <! :github/user))))

(defn elide-ast-nodes
  "Remove items from a query (AST) that have a key listed in the elision-set"
  [{:keys [key union-key] :as ast} elision-set]
  (let [union-elision? (contains? elision-set union-key)]
    (when-not (or union-elision? (contains? elision-set key))
      (update ast :children (fn [c] (vec (keep #(elide-ast-nodes % elision-set) c)))))))

(defn composed-query [{::keys [url q attr-handler]}]
  (go
    (let [without (-> attr-handler methods keys set (disj :default))
          q'      (-> q om/query->ast (elide-ast-nodes without) om/ast->query)
          json    (-> (gn/query #::gn{:url url :q q'}) <! ::gn/response-data)]
      (-> (gn/parse {::p/entity (.-data json)
                     ::p/reader [pa/js-obj-reader attr-handler]} q)
          (pa/read-chan-values)
          <!))))

(defrecord Network [url]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [_ edn ok error]
    (go
      (try
        (ok (<! (composed-query #::{:url url :q edn :attr-handler attr-handler})))
        (catch :default e
          (js/console.log "Network error" e)
          (error e)))))

  (start [_]))

(defn graphql-network [url]
  (map->Network {:url url}))

(defonce app
  (atom (fulcro/new-fulcro-client
          :started-callback (fn [{:keys [reconciler]}]
                              (fetch/load reconciler :app/all-contacts Contact)
                              #_(om/transact! reconciler `[(~'fulcro/load {:query   ~(om/focus-query (om/get-query Root) [:github/user])
                                                                           :marker  true
                                                                           :refresh [:github/user]})]))
          :networking (graphql-network "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"))))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(comment


  (go
    (-> #::{:q   [{:app/all-contacts
                   [:contact/id :contact/name :contact/github
                    {:contact/github-node [:user/bio :user/url]}]}]
            :url "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"}
        composed-query <! js/console.log))

  (let [remotes (-> attr-handler methods keys set (disj :default))
        q       [{:app/all-contacts
                  [:contact/id :contact/name :contact/github
                   {:contact/github-node [:user/bio]}]}]]
    (-> q om/query->ast (elide-ast-nodes remotes) om/ast->query))

  (om/get-query Root))
