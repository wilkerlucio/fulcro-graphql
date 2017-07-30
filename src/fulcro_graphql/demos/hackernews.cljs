(ns fulcro-graphql.demos.hackernews
  (:require [cljs.spec.alpha :as s]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [fulcro.client.mutations :as mutations]
            [fulcro-css.css :as css]
            [fulcro-graphql.styles :as style]
            [com.wsscode.pathom.graphql :as gql]
            [com.wsscode.fulcro-graphql.network :refer [graphql-network]]
            [om.dom :as dom]
            [om.next :as om])
  (:import [goog.Uri]))

(s/def :link/id (s/or :final string? :tmp om/tempid?))
(s/def :link/title string?)
(s/def :link/position pos-int?)
(s/def :link/url string?)
(s/def :link/points pos-int?)

(s/def :lifecycle/created-at inst?)

(declare LinkForm)

(defmethod mutations/mutate 'link/create-link [{:keys [state ast]} _ {:link/keys [title url id] :as link}]
  {:remote
   (assoc ast :params (select-keys link [:link/id :link/title :link/url]))

   :action
   (fn []
     (let [ref      [:link/by-id id]
           new-link (fulcro/get-initial-state LinkForm {})]
       (swap! state (comp #(assoc-in % ref {:link/id              id
                                            :link/title           title
                                            :link/url             url
                                            :link/position        (inc (count (get @state :link/all-links)))
                                            :link/points          1
                                            :lifecycle/created-at (js/Date.)})
                          #(update % :link/all-links conj ref)
                          #(assoc-in % [:link/by-id (:link/id new-link)] new-link)
                          #(assoc % :ui/link-form [:link/by-id (:link/id new-link)])))))})

(defn pd [f]
  (fn [e]
    (.preventDefault e)
    (f e)))

(defn url-domain [url]
  (-> (goog.Uri. url)
      .getDomain))

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defn icon [name]
  (dom/i #js {:className (str "fa fa-" name)}))

(defn input [{::keys [target field] :as props}]
  (dom/input (-> {:value    (some-> (om/props target) field)
                  :onChange #(mutations/set-string! target field :event %)}
                 (merge (into {} (filter (fn [[k _]] (simple-keyword? k))) props))
                 (clj->js))))

(om/defui ^:once LinkForm
  static fulcro/InitialAppState
  (initial-state [_ _] {:link/id    (om/tempid)
                        :link/url   "http://sample.com/site"
                        :link/title "Some Info"})

  static om/IQuery
  (query [_] [:link/id :link/url :link/title])

  static om/Ident
  (ident [_ props] [:link/by-id (:link/id props)])

  static css/CSS
  (local-rules [_] [[:.form-grid {:display "grid"
                                  :grid-template-columns "max-content auto"}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [props (om/props this)
          css   (css/get-classnames LinkForm)]
      (dom/form #js {:onSubmit (pd #(om/transact! this `[(link/create-link ~props)
                                                         :ui/link-form
                                                         :link/all-links]))}
        (dom/div #js {:className (:form-grid css)}
          (dom/label nil "title")
          (input {::target     this ::field :link/title
                  :placeholder "Title"})

          (dom/label nil "url")
          (input {::target     this ::field :link/url
                  :placeholder "Url"}))
        (dom/button #js {:type "submit"} "Create link")))))

(def link-form (om/factory LinkForm))

(om/defui ^:once UiLink
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:link/id :link/title :link/url :link/points :lifecycle/created-at])

  static om/Ident
  (ident [_ props] [:link/by-id (:link/id props)])

  static css/CSS
  (local-rules [_] [[:.container {:margin "8px 0"}]
                    [:.content {:margin-left "3px"}]
                    [:.position {:color      style/color-grey-828282
                                 :text-align "right"}]
                    [:.grey-text {:color style/color-grey-828282}]
                    [:.discrete (garden.selectors/> :.discrete "a")
                     {:color     style/color-grey-828282
                      :font-size style/font-8}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:link/keys [title position url points]
           :as        props} (om/props this)
          css (css/get-classnames UiLink)]
      (dom/div #js {:className (str "flex-row " (:container css))}
        (dom/div #js {:className (:position css)}
          (dom/span nil position) ". "
          (dom/a #js {:href "#" :className (:grey-text css)}
            (dom/img #js {:src "https://news.ycombinator.com/grayarrow.gif"})))
        (dom/div #js {:className (:content css)}
          (dom/div nil
            (dom/a #js {:href "#"} title)
            (dom/span #js {:className (:discrete css)}
              " (" (dom/a #js {:href url :className (:discrete css)} (url-domain url)) ")"))
          (dom/div #js {:className (str (:discrete css))}
            points " point" (if (not= 1 points) "s") " by "
            (dom/a #js {:href "#"} "author")
            (dom/a #js {:href "#"} " 6 minutes ago")))))))

(def ui-link (om/factory UiLink {:keyfn :link/id}))

(om/defui ^:once Header
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static css/CSS
  (local-rules [_] [[:.menu {:background style/color-orange-ff6600
                             :padding    "2px"}]
                    [:.title {:font-weight "bold"}]
                    [:.logo {:width  "18px" :height "18px"
                             :border (str "1px solid " style/color-white)}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:keys []} (om/props this)
          css (css/get-classnames Header)]
      (dom/div #js {:className (str (:menu css) " flex-row flex-align-center")}
        (dom/a #js {:href "#"} (dom/img #js {:src "https://news.ycombinator.com/y18.gif" :className (:logo css)}))
        (dom/a #js {:href "#" :className (:title css)} "Hacker News")
        (dom/div #js {:className "flex"})
        (dom/div nil
          (dom/a nil "wilkerlucio (5)")
          (dom/span nil " | ")
          (dom/a nil "logout"))))))

(def header (om/factory Header))

(om/defui ^:once Root
  static fulcro/InitialAppState
  (initial-state [_ _] {:ui/react-key   (random-uuid)
                        :ui/link-form   (fulcro/get-initial-state LinkForm {})
                        :link/all-links [#_ #_ #_{:link/id       (random-uuid)
                                          :link/title    "An Algebraic Language for the Manipulation of Symbolic Expressions (1958) [pdf]"
                                          :link/url      "http://www.softwarepreservation.org/projects/LISP/MIT/AIM-001.pdf"
                                          :link/position 1
                                          :link/points   1}
                                         {:link/id       (random-uuid)
                                          :link/title    "Why I left Medium and moved back to my own domain"
                                          :link/url      "https://arslan.io/2017/07/30/why-i-left-medium-and-moved-back-to-my-own-domain/"
                                          :link/position 2
                                          :link/points   2}
                                         {:link/id       (random-uuid)
                                          :link/title    "Ubershaders: A Ridiculous Solution to an Impossible Problem"
                                          :link/url      "https://dolphin-emu.org/blog/2017/07/30/ubershaders/"
                                          :link/position 3
                                          :link/points   1}]})

  static om/IQuery
  (query [_] [{:link/all-links (get-load-query UiLink)}
              {:ui/link-form (om/get-query LinkForm)}
              :ui/react-key])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [UiLink Header LinkForm])

  static css/Global
  (global-rules [_] style/global-styles)

  Object
  (render [this]
    (let [{:keys [link/all-links ui/react-key] :as props} (om/props this)
          css (css/get-classnames Root)]
      (dom/div #js {:key react-key}
        (dom/div #js {:className "container content-background"}
          (header {})

          (if (fetch/loading? (:ui/fetch-state all-links))
            (dom/div nil "Loading..."))
          (if (sequential? all-links)
            (map-indexed (fn [i l] (ui-link (assoc l :link/position (inc i)))) all-links))

          (dom/hr nil)

          (link-form (:ui/link-form props)))))))

(defonce app
  (atom (fulcro/new-fulcro-client
          :networking (graphql-network "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0")
          :started-callback (fn [app] (fetch/load app :link/all-links UiLink)))))

(defn init [] (swap! app fulcro/mount Root "app-container"))

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(css/upsert-css "hackernews" Root)
