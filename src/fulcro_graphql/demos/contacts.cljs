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
       (swap! state (comp #(update-in % [:app/all-contacts] conj ref)
                          #(assoc-in % [:Contact/by-id (:contact/id new-user)] new-user)
                          #(assoc % :ui/new-user [:Contact/by-id (:contact/id new-user)])))))})

(defn not-found [x default]
  (if (= x :fulcro.client.impl.om-plumbing/not-found)
    default x))

(om/defui ^:once Contact
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:contact/id :contact/github
              {:contact/github-node
               [:github/avatar-url :github/name :github/company
                '({:github/repositories [{:nodes [:url]}]} {:last 5})]}])

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
        (dom/div nil github)
        (dom/div nil (not-found (:github/name github-node) ""))
        #_#_(dom/div nil (:github/company github-node))
            (dom/div nil
              (for [{:keys [url]} (get-in github-node [:github/repositories :nodes])]
                (dom/div nil "Repo: " url)))))))

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
    (let [{:keys [contact/github] :as props} (om/props this)
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

(om/defui ^:once GroupView
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:group/id :group/name {:group/contacts (om/get-query Contact)}])

  static om/Ident
  (ident [_ props] [:Group/by-id (:group/id props)])

  static css/CSS
  (local-rules [_] [[:.contacts {:display               "grid"
                                 :grid-template-columns "repeat(5, 1fr)"}]])
  (include-children [_] [Contact])

  Object
  (render [this]
    (let [{:group/keys [name contacts]} (om/props this)
          css (css/get-classnames GroupView)]
      (dom/div nil
        (str name)
        (dom/div #js {:className (:contacts css)}
          (map contact contacts))))))

(def group-view (om/factory GroupView))

(om/defui ^:once GroupItem
  static fulcro/InitialAppState
  (initial-state [_ _] {:group/id   (om/tempid)
                        :group/name ""})

  static om/IQuery
  (query [_] [:group/id :group/name])

  static om/Ident
  (ident [_ props] [:Group/by-id (:group/id props)])

  static css/CSS
  (local-rules [_] [[:.container {:cursor "pointer"}]
                    [:.selected {:background "#ccc"}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:group/keys [name] :as props} (om/props this)
          {:keys [event/on-select ui/selected?]} (om/get-computed props)
          css (css/get-classnames GroupItem)]
      (dom/div #js {:onClick   #(on-select props)
                    :className (cond-> (:container css)
                                 selected? (str " " (:selected css)))} name))))

(def group-item (om/factory GroupItem))

(om/defui ^:once Contacts
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [{:app/all-groups (om/get-query GroupItem)}
              {:app/selected-group (om/get-query GroupView)}])

  static om/Ident
  (ident [_ props] [:contact-app/instance "main"])

  static css/CSS
  (local-rules [_] [[:.container {:display "flex"}]])
  (include-children [_] [GroupItem GroupView])

  static css/Global
  (global-rules [_] [[:body {:background style/color-white}]
                     [:.flex-expand {:flex "1"}]])

  Object
  (render [this]
    (let [{:keys [app/all-groups app/selected-group]} (om/props this)
          css (css/get-classnames Contacts)]
      (dom/div #js {:className (:container css)}
        (dom/div nil
          (map (comp group-item
                     #(om/computed % {:ui/selected?    (= (:group/id selected-group) (:group/id %))
                                      :event/on-select (fn [{:keys [group/id]}]
                                                         (fetch/load this [:Group/by-id id] GroupView)
                                                         (mutations/set-value! this :app/selected-group [:Group/by-id id]))}))
               all-groups))
        (dom/div #js {:className "flex-expand"}
          (if selected-group
            (group-view selected-group)
            "No group selected"))))))

(def contacts-ui (om/factory Contacts))

(om/defui ^:once Root
  static fulcro/InitialAppState
  (initial-state [_ _] {:ui/react-key (random-uuid)
                        :app/contacts (fulcro/get-initial-state Contacts {})})

  static om/IQuery
  (query [_] [{:app/contacts (om/get-query Contacts)}
              :ui/react-key])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [Contacts])

  Object
  (render [this]
    (let [{:keys [ui/react-key app/contacts]} (om/props this)]
      (dom/div #js {:key react-key}
        (contacts-ui contacts)))))

(declare composed-query)

(defn elide-ast-nodes
  "Remove items from a query (AST) that have a key listed in the elision-set"
  [{:keys [key union-key] :as ast} elision-set]
  (let [union-elision? (contains? elision-set union-key)]
    (when-not (or union-elision? (contains? elision-set key))
      (update ast :children (fn [c] (if c (vec (keep #(elide-ast-nodes % elision-set) c))))))))

(defn ident-reader [{:keys [ast]
                     :as   env}]
  (if (vector? (:key ast))
    (let [e (p/entity env)]
      (pa/read-chan-values (p/join (gobj/get e (gql/ident->alias (:key ast)))
                                   env)))
    ::p/continue))

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

(defn composed-query [{::keys [url q attr-handler]}]
  (go
    (let [without (-> attr-handler methods keys set (disj :default))
          q'      (-> q om/query->ast (elide-ast-nodes without) om/ast->query)
          json    (-> (gn/query #::gn{:url url :q q'}) <! ::gn/response-data)]
      (-> (gn/parse {::p/entity (.-data json)
                     ::p/reader [pa/js-obj-reader attr-handler ident-reader]} q)
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
                              (fetch/load reconciler :app/all-groups GroupItem {:target [:contact-app/instance "main" :app/all-groups]}))
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
    (-> #::{:q            [{:app/all-contacts
                            [:contact/id :contact/github
                             {:contact/github-node [:user/avatar-url]}]}]
            :url          "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"
            :attr-handler attr-handler}
        composed-query <! js/console.log))

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

  (gql/ident->alias [:Contact/by-id "cj6l0c526011j012989kbdzhh"])

  (om/get-query Root))
