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
            [fulcro.client.mutations :as mutations :include-macros true]
            [fulcro.client.data-fetch :as fetch]
            [com.wsscode.common.local-storage :as local-storage]
            [fulcro.client.network :as fulcro.network]))

(defn get-token []
  (if-let [token (local-storage/get "github-token")]
    token
    (let [token (js/prompt "Please enter github token:")]
      (local-storage/set! "github-token" token)
      token)))

(declare AddUserForm)

(defmethod mutations/mutate `create-contact [{:keys [state ast]} _ {:contact/keys [id github] :as contact}]
  {:remote
   (assoc ast :params (select-keys contact [:contact/id :contact/github]))

   :action
   (fn []
     (let [ref      [:Contact/by-id id]
           new-user (fulcro/get-initial-state AddUserForm {})]
       (swap! state (comp #(assoc-in % ref {:contact/id     id
                                            :contact/github github})
                          #(update-in % [:app/all-contacts] conj ref)
                          #(assoc-in % [:Contact/by-id (:contact/id new-user)] new-user)
                          #(assoc % :ui/new-user [:Contact/by-id (:contact/id new-user)])))))})

(om/defui ^:once Contact
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:contact/id :contact/github
              {:contact/github-node [:github/avatar-url]}])

  static om/Ident
  (ident [_ props] [:Contact/by-id (:contact/id props)])

  static css/CSS
  (local-rules [_] [[:.avatar {:width "100px" :height "100px"}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:contact/keys [github github-node]} (om/props this)
          css (css/get-classnames Contact)]
      (dom/div nil
        (dom/div nil
          (dom/img #js {:className (:avatar css)
                        :src       (:github/avatar-url github-node)}))
        (dom/div nil github)))))

(def contact (om/factory Contact))

(om/defui ^:once AddUserForm
  static fulcro/InitialAppState
  (initial-state [_ _] {:contact/id     (om/tempid)
                        :contact/github ""})

  static om/IQuery
  (query [_] [:contact/id :contact/github])

  static om/Ident
  (ident [_ props] [:Contact/by-id (:contact/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:keys [contact/github contact/id] :as props} (om/props this)
          css (css/get-classnames AddUserForm)]
      (dom/div nil
        (dom/input #js {:type     "text"
                        :value    github
                        :onChange #(mutations/set-string! this :contact/github :event %)})
        (dom/button #js {:onClick #(do
                                     (om/transact! this [`(create-contact ~props)
                                                         :ui/new-user
                                                         :app/all-contacts])
                                     (fetch/load this (om/get-ident this) Contact))}
          "Add")))))

(def add-user-form (om/factory AddUserForm))

(om/defui ^:once Root
  static fulcro/InitialAppState
  (initial-state [_ _] {:ui/react-key (random-uuid)
                        :ui/new-user  (fulcro/get-initial-state AddUserForm {})})

  static om/IQuery
  (query [_] [{:app/all-contacts (om/get-query Contact)}
              {:ui/new-user (om/get-query AddUserForm)}
              :ui/react-key])

  static css/CSS
  (local-rules [_] [[:.contacts {:display               "grid"
                                 :grid-template-columns "repeat(5, 1fr)"}]])
  (include-children [_] [Contact])

  static css/Global
  (global-rules [_] [[:body {:background style/color-white}]])

  Object
  (render [this]
    (let [{:keys [app/all-contacts ui/react-key ui/new-user]} (om/props this)
          css (css/get-classnames Root)]
      (dom/div #js {:key react-key}
        (add-user-form new-user)
        (dom/hr nil)
        (dom/div #js {:className (:contacts css)}
          (map contact all-contacts))))))

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
      (update ast :children (fn [c] (if c (vec (keep #(elide-ast-nodes % elision-set) c))))))))

(defn ident-reader [{::gql/keys [ident-counter]
                     :keys      [ast]
                     :as        env}]
  (if (vector? (:key ast))
    (let [e (p/entity env)]
      (pa/read-chan-values (p/join (gobj/get e (str "pathomId" (swap! ident-counter inc)))
                                   env)))
    ::p/continue))

(defn composed-query [{::keys [url q attr-handler]}]
  (go
    (let [without (-> attr-handler methods keys set (disj :default))
          q'      (-> q om/query->ast (elide-ast-nodes without) om/ast->query)
          json    (-> (gn/query #::gn{:url url :q q'}) <! ::gn/response-data)]
      (-> (gn/parse {::p/entity          (.-data json)
                     ::gql/ident-counter (atom 0)
                     ::p/reader          [pa/js-obj-reader attr-handler ident-reader]} q)
          (pa/read-chan-values)
          <!
          (gn/lift-tempids)))))

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

(css/upsert-css "demo-contacts" Root)

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(comment

  (println (gql/query->graphql `[(create-contact {:contact/id ~(om/tempid) :contact/github "bla"})]
                               {::gql/js-name gn/js-name}))

  (-> `[(create-contact {:contact/id ~(om/tempid) :contact/github "bla"})]
      (om/query->ast)
      (om/ast->query))

  (-> `[{(create-contact {:contact/id ~(om/tempid), :contact/github "caioaao"}) [:ui]}]
      (fulcro.client.impl.om-plumbing/strip-ui)
      #_pr-str)

  (-> `{:dispatch-key create-contact, :key create-contact, :params {:contact/id ~(om/tempid), :contact/github "ccc"}, :type :call
        :children     [{:type :prop, :dispatch-key :id, :key :id}]}
      (om/ast->query))

  (go
    (-> #::{:q            [{[:Contact/by-id "cj6l0c526011j012989kbdzhh"]
                            [:contact/id :contact/github
                             {:contact/github-node [:user/avatar-url]}]}
                           {[:Contact/by-id "cj6l0cdch012b0186yf6wcmvw"]
                            [:contact/id :contact/github]}]
            :url          "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"
            :attr-handler attr-handler}
        composed-query <! js/console.log))

  (let [remotes (-> attr-handler methods keys set (disj :default))
        q       [{:app/all-contacts
                  [:contact/id :contact/name :contact/github
                   {:contact/github-node [:user/bio]}]}]]
    (-> q om/query->ast (elide-ast-nodes remotes) om/ast->query))

  (om/get-query Root))
