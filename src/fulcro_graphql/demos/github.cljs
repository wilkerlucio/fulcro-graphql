(ns fulcro-graphql.demos.github
  (:require [cljs.spec.alpha :as s]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [fulcro.client.mutations :as fm]
            [fulcro-css.css :as css]
            [fulcro-graphql.styles :as style]
            [com.wsscode.common.local-storage :as local-storage]
            [com.wsscode.pathom.graphql :as gql]
            [com.wsscode.fulcro-graphql.network :as gn :refer [graphql-network]]
            [om.dom :as dom]
            [om.next :as om]))

(defn get-token []
  (if-let [token (local-storage/get "github-token")]
    token
    (let [token (js/prompt "Please enter github token:")]
      (local-storage/set! "github-token" token)
      token)))

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defmethod fm/mutate 'repository/add-star [{:keys [state ast]} _ {:keys [repository/id]}]
  {:remote
   (assoc ast :params {:input            {:starrable-id id}
                       ::gql/mutate-join [:client-mutation-id]})

   :action
   (fn []
     (swap! state assoc-in [:repository/by-id id :repository/viewer-has-starred] true))})

(defmethod fm/mutate 'repository/remove-star [{:keys [state ast]} _ {:keys [repository/id]}]
  {:remote
   (assoc ast :params {:input            {:starrable-id id}
                       ::gql/mutate-join [:client-mutation-id]})

   :action
   (fn []
     (swap! state assoc-in [:repository/by-id id :repository/viewer-has-starred] false))})

(om/defui ^:once Repository
  static om/IQuery
  (query [_] [:repository/id :repository/name :repository/description :repository/url
              :repository/viewer-has-starred])

  static om/Ident
  (ident [_ props] [:repository/by-id (:repository/id props)])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:repository/keys [name description viewer-has-starred id]
           :as              props} (om/props this)]
      (dom/div nil
        (dom/div nil name)
        (dom/div nil id " - " description)
        (if viewer-has-starred
          (dom/button #js {:onClick #(om/transact! this `[(repository/remove-star ~props)])}
            "Unstar")

          (dom/button #js {:onClick #(om/transact! this `[(repository/add-star ~props)])}
            "Star"))))))

(def ui-link (om/factory Repository {:keyfn :repository/id}))

(declare app)

(om/defui ^:once Root
  static fulcro/InitialAppState
  (initial-state [_ _] {:ui/react-key (random-uuid)})

  static om/IQuery
  (query [_] `[({:github/user
                 [({:user/repositories
                    [{:nodes ~(om/get-query Repository)}]}
                    {:last 10})
                  :ui/fetch-state]}
                 {:user/login "wilkerlucio"})

               :ui/react-key])

  static css/Global
  (global-rules [_] [[:body {:background (style/color-white)}]])

  Object
  (render [this]
    (let [{:keys [github/user ui/react-key]} (om/props this)
          repositories (get-in user [:user/repositories :nodes])]
      (dom/div #js {:key react-key}
        (if (fetch/loading? (:ui/fetch-state user))
          (dom/div nil "Loading..."))
        (if (sequential? repositories)
          (map ui-link repositories))))))

(defonce app
  (atom (fulcro/new-fulcro-client
          :started-callback (fn [{:keys [reconciler]}]
                              (om/transact! reconciler `[(~'fulcro/load {:query   ~(om/focus-query (om/get-query Root) [:github/user])
                                                                         :marker  true
                                                                         :refresh [:github/user]})]))
          :networking (graphql-network (str "https://api.github.com/graphql?access_token=" (get-token))))))

(defn init []
  (swap! app fulcro/mount Root "gh-app-container"))

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(comment
  (om/ast->query {:dispatch-key 'repository/add-star
                  :key          'repository/add-star
                  :params       {:repository/id  "MDEwOlJlcG9zaXRvcnk5NzAzMTc0NQ==", :repository/name "untangled", :repository/description
                                                 "A library for buildi … lojure/ClojureScript"
                                 :repository/url "https://github.com/wilkerlucio/untangled", :repository/viewer-has-starred false}
                  :type         :join
                  :children     (-> (om/query->ast [:client-mutation-id]) :children)

                  ::gql/js-name gn/js-name})

  (gql/node->graphql `{:dispatch-key some/mutation,
                       :key          some/mutation,
                       :params       {:param/a 1, :param/b 2},
                       :type         :join,
                       :query        [:child-query],
                       :children     [{:type :prop, :dispatch-key :child-query, :key :child-query}]

                       ::gql/js-name ~name})

  (om/focus-query (om/get-query Root) [:github/user]))
