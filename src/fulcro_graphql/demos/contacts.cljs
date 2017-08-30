(ns fulcro-graphql.demos.contacts
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.next :as om]
            [om.dom :as dom]
            [goog.object :as gobj]
            [cljs.core.async :refer [<!]]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.async :as pa]
            [com.wsscode.pathom.graphql :as gql]
            [com.wsscode.pathom.fulcro :refer [batch-network]]
            [com.wsscode.fulcro-graphql.network :as gn]
            [fulcro-graphql.styles :as style]
            [fulcro-graphql.models.contacts.contact :as c.contact]
            [fulcro-graphql.models.contacts.group :as c.group]
            [fulcro-graphql.models.contacts.repository :as c.repository]
            [fulcro-graphql.models.github.repository :as g.repository]
            [fulcro-graphql.models.github.user :as g.user]
            [fulcro-css.css :as css]
            [fulcro.client.core :as fulcro]
            [fulcro.client.mutations :as mutations :include-macros true]
            [fulcro.client.data-fetch :as fetch]
            [com.wsscode.common.local-storage :as local-storage]
            [fulcro.client.network :as fulcro.network]
            [clojure.core.async :as async]
            [clojure.string :as str]))

(defn get-token []
  (if-let [token (local-storage/get "github-token")]
    token
    (let [token (js/prompt "Please enter github token:")]
      (local-storage/set! "github-token" token)
      token)))

(declare AddUserForm GroupView)

(defmethod mutations/mutate `add-to-group-on-contact [{:keys [ast]} _ params]
  {:remote
   (assoc ast :params (assoc params ::gql/mutate-join [{:contacts-contact [:id]}
                                                       {:groups-group [:id]}]))})

(defmethod mutations/mutate `create-contact [{:keys [state ast]} _ {::c.contact/keys [id group-id github] :as contact}]
  {:remote
   (assoc ast :params (select-keys contact [::c.contact/id ::c.contact/github]))

   :action
   (fn []
     (let [ref       [:Contact/by-id id]
           group-ref [:Group/by-id group-id]
           new-user  (fulcro/get-initial-state AddUserForm {})]
       (swap! state (comp #(update-in % (conj group-ref ::c.group/contacts) conj ref)
                          #(assoc-in % (conj ref ::c.contact/github-user) [:github.user/by-login github])
                          #(assoc-in % [:Contact/by-id (::c.contact/id new-user)] new-user)
                          #(assoc-in % (conj group-ref :ui/new-contact) [:Contact/by-id (::c.contact/id new-user)])))))})

(defmethod mutations/mutate `create-group [{:keys [state ast ref]} _ {::c.group/keys [id] :as group}]
  {:remote
   (assoc ast :params (select-keys group [::c.group/id ::c.group/name]))

   :action
   (fn []
     (let [group-ref [:Group/by-id id]]
       (swap! state (comp #(update-in % (conj ref :app/all-groups) conj group-ref)
                          #(assoc-in % group-ref group)))))})

(defmethod mutations/mutate `update-group [{:keys [state ast]} _ {::c.group/keys [id] :as group}]
  {:remote
   (assoc ast :params (-> (select-keys group [::c.group/id ::c.group/name])
                          (assoc ::gql/mutate-join [:id])))

   :action
   (fn []
     (let [group-ref [:Group/by-id id]]
       (swap! state assoc-in group-ref group)))})

(defmethod mutations/mutate `select-group [{:keys [state reconciler ref]} _ {:keys [::c.group/id]}]
  {:action
   (fn []
     (if-not (get-in @state [:Group/by-id id :ui/new-contact])
       (let [new-user    (fulcro/get-initial-state AddUserForm {})
             contact-ref [:Contact/by-id (::c.contact/id new-user)]]
         (swap! state (comp #(assoc-in % [:Group/by-id id :ui/new-contact] contact-ref)
                            #(assoc-in % contact-ref new-user)))))

     (fetch/load reconciler [:Group/by-id id] GroupView)

     (swap! state assoc-in (conj ref :app/selected-group) [:Group/by-id id]))})

(defn not-found [x default]
  (if (= x :fulcro.client.impl.om-plumbing/not-found)
    default x))

(om/defui ^:once GithubUserView
  static om/IQuery
  (query [_] [::g.user/login ::g.user/avatar-url])

  static om/Ident
  (ident [_ props] [:github.user/by-login (::g.user/login props)])

  static css/CSS
  (local-rules [_] [[:.container {:text-align "center"}]
                    [:.avatar {:width "100px" :height "100px"}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{::g.user/keys [avatar-url login]} (om/props this)
          css (css/get-classnames GithubUserView)]
      (dom/div #js {:className (:container css)}
        (dom/div nil
          (dom/img #js {:className (:avatar css)
                        :src       avatar-url}))
        (dom/div nil login)))))

(def github-user-view (om/factory GithubUserView))

(om/defui ^:once Contact
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [::c.contact/id ::c.contact/github
              {::c.contact/github-user (om/get-query GithubUserView)}])

  static om/Ident
  (ident [_ props] [:Contact/by-id (::c.contact/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [GithubUserView])

  Object
  (render [this]
    (let [{::c.contact/keys [github-user]} (om/props this)
          css (css/get-classnames Contact)]
      (github-user-view github-user))))

(def contact (om/factory Contact))

(om/defui ^:once AddUserForm
  static fulcro/InitialAppState
  (initial-state [_ _] {::c.contact/id     (om/tempid)
                        ::c.contact/github ""})

  static om/IQuery
  (query [_] [::c.contact/id ::c.contact/github])

  static om/Ident
  (ident [_ props] [:Contact/by-id (::c.contact/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:keys [::c.contact/github] :as props} (om/props this)
          {:keys [::c.group/id]} (om/get-computed props)
          css (css/get-classnames AddUserForm)]
      (dom/form #js {:className "form-row align-items-center"
                     :onSubmit  (fn [e]
                                  (.preventDefault e)
                                  (om/transact! this [`(create-contact ~(assoc props ::c.contact/group-id id))
                                                      :ui/new-user
                                                      ::c.group/contacts])
                                  (fetch/load this [:github.user/by-login github] GithubUserView {:remote  :github
                                                                                                  :refresh [::c.contact/github]})
                                  (js/setTimeout
                                    (fn []
                                      (om/transact! this [`(add-to-group-on-contact {:contacts-contact-id ~(::c.contact/id props)
                                                                                     :groups-group-id     ~id})]))
                                    10))}
        (dom/div #js {:className "col-auto"}
          (dom/input #js {:type        "text"
                          :value       github
                          :placeholder "Github user name"
                          :className   "form-control"
                          :onChange    #(mutations/set-string! this ::c.contact/github :event %)}))
        (dom/div #js {:className "col-auto"}
          (dom/button #js {:className "btn btn-primary"
                           :type      "submit"}
            "Add"))))))

(def add-user-form (om/factory AddUserForm))

(om/defui ^:once Repository
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [::c.repository/id ::c.repository/name
              {::c.repository/github
               [::g.repository/name {::g.repository/owner (om/get-query GithubUserView)}]}])

  static om/Ident
  (ident [_ props] [:Repository/by-id (::c.repository/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [GithubUserView])

  Object
  (render [this]
    (let [{::c.repository/keys [name github]} (om/props this)
          css (css/get-classnames Repository)]
      (dom/div nil
        (dom/div nil name)
        (github-user-view (-> github ::g.repository/owner))))))

(def repository (om/factory Repository))

(om/defui ^:once RepositoryToPick
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [::g.repository/name-with-owner ::g.repository/url])

  static om/Ident
  (ident [_ props]
    (let [[owner name] (str/split (::g.repository/name-with-owner props) "/")]
      [:github.repository/by-owner-and-name [owner name]]))

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [])

  Object
  (render [this]
    (let [{::g.repository/keys [url]} (om/props this)
          css (css/get-classnames RepositoryToPick)]
      (dom/div nil url))))

(def repository-to-pick (om/factory RepositoryToPick))

(om/defui ^:once GithubStarredReposPicker
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [::g.user/login
              `({::g.user/starred-repositories [{:nodes ~(om/get-query RepositoryToPick)}]} {:last 10})])

  static om/Ident
  (ident [_ props] [:github.user/by-login (::g.user/login props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [RepositoryToPick])

  Object
  (render [this]
    (let [{::g.user/keys [starred-repositories] :as props} (om/props this)
          css (css/get-classnames GithubStarredReposPicker)]
      (dom/div nil
        (dom/h2 nil "Add repository")
        (->> starred-repositories :nodes (map repository-to-pick))))))

(def github-starred-repos-picker (om/factory GithubStarredReposPicker))

(om/defui ^:once GroupView
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [::c.group/id ::c.group/name :ui/fetch-state
              {::c.group/repositories (om/get-query Repository)}
              {:github.root/viewer (om/get-query GithubStarredReposPicker)}])

  static om/Ident
  (ident [_ props] [:Group/by-id (::c.group/id props)])

  static css/CSS
  (local-rules [_] [[:.contacts {:display               "grid"
                                 :grid-template-columns "repeat(5, 1fr)"
                                 :justify-items         "center"
                                 :grid-gap              "26px"}]
                    [:.title {:cursor "pointer"}]])
  (include-children [_] [Contact AddUserForm Repository GithubStarredReposPicker])

  Object
  (render [this]
    (let [{::c.group/keys [name repositories]
           :ui/keys       [fetch-state]
           :keys          [github.root/viewer]
           :as            props} (om/props this)
          css (css/get-classnames GroupView)]
      (dom/div nil
        (dom/h1 #js {:className (:title css)}
                (dom/a #js {:onClick #(if-let [new-name (js/prompt "New group name" name)]
                                        (om/transact! this [`(update-group ~(assoc props ::c.group/name new-name))]))}
                  (str name)))
        (if (fetch/loading? fetch-state)
          "Loading...")
        (dom/div #js {:className (:contacts css)}
          (->> repositories
               (sort-by ::c.repository/name)
               (map repository)))
        (if viewer
          (github-starred-repos-picker viewer))))))

(def group-view (om/factory GroupView))

(om/defui ^:once GroupMenuItem
  static fulcro/InitialAppState
  (initial-state [_ _] {::c.group/id   (om/tempid)
                        ::c.group/name ""})

  static om/IQuery
  (query [_] [::c.group/id ::c.group/name])

  static om/Ident
  (ident [_ props] [:Group/by-id (::c.group/id props)])

  static css/CSS
  (local-rules [_] [[:.container {:cursor "pointer"}]
                    [:.selected {:background "#ccc"}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{::c.group/keys [name] :as props} (om/props this)
          {:keys [event/on-select ui/selected?]} (om/get-computed props)
          css (css/get-classnames GroupMenuItem)]
      (dom/div #js {:onClick   #(on-select props)
                    :className (cond-> (:container css)
                                 selected? (str " " (:selected css)))} name))))

(def group-menu-item (om/factory GroupMenuItem))

(om/defui ^:once Contacts
  static fulcro/InitialAppState
  (initial-state [_ _] {:app/all-groups []})

  static om/IQuery
  (query [_] [{:app/all-groups (om/get-query GroupMenuItem)}
              {:app/selected-group (om/get-query GroupView)}])

  static om/Ident
  (ident [_ props] [:contact-app/instance "main"])

  static css/CSS
  (local-rules [_] [[:.container {:display               "grid"
                                  :grid-template-columns "auto 1fr"
                                  :grid-gap              "20px"}]
                    [:.group-menu {:padding "10px"}]
                    [:.group-view {:flex "1"}]])
  (include-children [_] [GroupMenuItem GroupView])

  static css/Global
  (global-rules [_] [[:body {:background style/color-white}]
                     [:.flex-expand {:flex "1"}]])

  Object
  (render [this]
    (let [{:keys [app/all-groups app/selected-group]} (om/props this)
          css (css/get-classnames Contacts)]
      (dom/div #js {:className (:container css)}
        (dom/div #js {:className (:group-menu css)}
          (dom/button #js {:className "btn btn-primary"
                           :onClick   #(if-let [name (js/prompt "New group name")]
                                         (om/transact! this [`(create-group {::c.group/id   ~(om/tempid)
                                                                             ::c.group/name ~name})]))}
            "New Group")
          (dom/br nil)
          (dom/br nil)
          (map (comp group-menu-item
                     #(om/computed % {:ui/selected?    (= (::c.group/id selected-group) (::c.group/id %))
                                      :event/on-select (fn [group] (om/transact! this [`(select-group ~group)]))}))
               (sort-by ::c.group/name all-groups)))
        (dom/div #js {:className (:group-view css)}
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

(defn join-remote [{::keys [app remote join-root]
                    :keys  [query]}]
  (let [c (async/promise-chan)]
    (go
      (if-let [network (-> app :networking (get remote))]
        (fulcro.network/send network [{join-root query}] #(async/put! c (get % join-root)) #(async/put! c %))
        (do
          (js/console.warn "Invalid remote" {:remote remote})
          (async/close! c))))
    c))

(defmethod attr-handler ::c.contact/github-user [{:keys [::p/entity] :as env}]
  (let [github (gobj/get entity "github")]
    (join-remote (assoc env ::join-root [:user/by-login github] ::remote :github))))

(defmethod attr-handler ::g.user/contact [{:keys [::p/entity] :as env}]
  (let [github (gobj/get entity "login")]
    (join-remote (assoc env ::join-root [:Contact/by-github github] ::remote :remote))))

(defmethod attr-handler ::c.repository/github [{:keys [::p/entity] :as env}]
  (let [[owner name] (-> (gobj/get entity "name") (str/split #"/"))]
    (join-remote (assoc env ::join-root [::g.repository/by-owner-and-name [owner name]] ::remote :github))))

(defmethod attr-handler :github.root/viewer [env]
  (join-remote (assoc env ::join-root :viewer ::remote :github)))

(defn composed-query [{::keys [url q attr-handler app]}]
  (go
    (let [without (-> attr-handler methods keys set (disj :default))
          q'      (-> q om/query->ast (elide-ast-nodes without) om/ast->query)
          json    (-> (gn/query #::gn{:url url :q q'}) <! ::gn/response-data)]
      (-> (gn/parse {::p/entity (.-data json)
                     ::p/reader [pa/js-obj-reader attr-handler ident-reader]
                     ::app      app} q)
          (pa/read-chan-values)
          <!
          (gn/lift-tempids)))))

(defrecord Network [url app]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [_ edn ok error]
    (go
      (ok (<! (composed-query #::{:url url :q edn :attr-handler attr-handler :app @app})))))

  (start [_]))

(defn graphql-network [url app]
  (map->Network {:url url :app app}))

(defonce app (atom nil))

(defonce start-app
  (reset! app
          (fulcro/new-fulcro-client
            :started-callback (fn [{:keys [reconciler]}]
                                (fetch/load reconciler :app/all-groups GroupMenuItem {:target [:contact-app/instance "main" :app/all-groups]}))
            :networking {:remote (-> (graphql-network "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5" app)
                                     (batch-network))
                         :github (-> (graphql-network (str "https://api.github.com/graphql?access_token=" (get-token)) app)
                                     (batch-network))})))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(css/upsert-css "demo-contacts" Root)

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(comment
  (gql/ident-transform [:github.user/by-login "wilker"])

  (println (gql/query->graphql `[(create-contact {::c.contact/id ~(om/tempid) ::c.contact/github "bla"})]
                               {::gql/js-name gn/js-name}))

  (-> `[(create-contact {::c.contact/id ~(om/tempid) ::c.contact/github "bla"})]
      (om/query->ast)
      (om/ast->query))

  (-> `[{(create-contact {::c.contact/id ~(om/tempid), ::c.contact/github "caioaao"}) [:ui]}]
      (fulcro.client.impl.om-plumbing/strip-ui)
      #_pr-str)

  (-> `{:dispatch-key create-contact, :key create-contact, :params {::c.contact/id ~(om/tempid), ::c.contact/github "ccc"}, :type :call
        :children     [{:type :prop, :dispatch-key :id, :key :id}]}
      (om/ast->query))

  (go
    (-> #::{:q            [{:app/all-contacts
                            [::c.contact/id ::c.contact/github
                             {::c.contact/github-user [:user/avatar-url]}]}]
            :url          "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"
            :attr-handler attr-handler}
        composed-query <! js/console.log))

  (go
    (-> #::{:q            [{[:Contact/by-id "cj6l0c526011j012989kbdzhh"]
                            [::c.contact/id ::c.contact/github
                             {::c.contact/github-user [:user/avatar-url]}]}
                           {[:Contact/by-id "cj6l0cdch012b0186yf6wcmvw"]
                            [::c.contact/id ::c.contact/github]}]
            :url          "https://api.graph.cool/simple/v1/cj6h5p18026ba0110ogeyn1o5"
            :attr-handler attr-handler}
        composed-query <! js/console.log))

  (let [remotes (-> attr-handler methods keys set (disj :default))
        q       [{:app/all-contacts
                  [::c.contact/id ::c.contact/name ::c.contact/github
                   {::c.contact/github-user [:user/bio]}]}]]
    (-> q om/query->ast (elide-ast-nodes remotes) om/ast->query))

  (gql/ident->alias [:Contact/by-id "cj6l0c526011j012989kbdzhh"])

  (om/get-query Root))
