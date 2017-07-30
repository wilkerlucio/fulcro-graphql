(ns fulcro-graphql.demos.hackernews
  (:require [cljs.spec.alpha :as s]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [fulcro-css.css :as css]
            [fulcro-graphql.styles :as style]
            [com.wsscode.fulcro-graphql.network :refer [graphql-network]]
            [om.dom :as dom]
            [om.next :as om])
  (:import [goog.Uri]))

(defn url-domain [url]
  (-> (goog.Uri. url)
      .getDomain))

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defn icon [name]
  (dom/i #js {:className (str "fa fa-" name)}))

(om/defui ^:once UiLink
  static fulcro/InitialAppState
  (initial-state [_ _] {})

  static om/IQuery
  (query [_] [:link/id :link/description :link/position :link/url])

  static om/Ident
  (ident [_ props] [:link/by-id (:link/id props)])

  static css/CSS
  (local-rules [_] [[:.container {:margin "5px 0"}]
                    [:.position {:color style/color-grey-828282
                                 :text-align "right"}]
                    [:.grey-text {:color style/color-grey-828282}]
                    [:.discrete (garden.selectors/> :.discrete "a")
                     {:color     style/color-grey-828282
                                 :font-size style/font-8}]])
  (include-children [_] [])

  Object
  (render [this]
    (let [{:link/keys [description position url]
           :as        props} (om/props this)
          css (css/get-classnames UiLink)]
      (dom/div #js {:className (str "flex-row " (:container css))}
        (dom/div #js {:className (:position css)}
          (dom/span nil position) ". "
          (dom/a #js {:href "#" :className (:grey-text css)} (icon "sort-asc")))
        (dom/div nil
          (dom/div nil
            (dom/a #js {:href "#"} description)
            (dom/span #js {:className (:discrete css)}
              " (" (dom/a #js {:href url :className (:discrete css)} (url-domain url)) ")"))
          (dom/div #js {:className (:discrete css)}
            "1 point by "
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
                        :link/all-links [{:link/id          (random-uuid)
                                          :link/description "An Algebraic Language for the Manipulation of Symbolic Expressions (1958) [pdf]"
                                          :link/url         "http://www.softwarepreservation.org/projects/LISP/MIT/AIM-001.pdf"
                                          :link/position    1}
                                         {:link/id          (random-uuid)
                                          :link/description "Why I left Medium and moved back to my own domain"
                                          :link/url         "https://arslan.io/2017/07/30/why-i-left-medium-and-moved-back-to-my-own-domain/"
                                          :link/position    2}
                                         {:link/id          (random-uuid)
                                          :link/description "Ubershaders: A Ridiculous Solution to an Impossible Problem"
                                          :link/url         "https://dolphin-emu.org/blog/2017/07/30/ubershaders/"
                                          :link/position    3}]})

  static om/IQuery
  (query [_] [{:link/all-links (get-load-query UiLink)}
              :ui/react-key])

  static css/CSS
  (local-rules [_] [])
  (include-children [_] [UiLink Header])

  static css/Global
  (global-rules [_] style/global-styles)

  Object
  (render [this]
    (let [{:keys [link/all-links ui/react-key]} (om/props this)
          css (css/get-classnames Root)]
      (dom/div #js {:key react-key}
        (dom/div #js {:className "container content-background"}
          (header {})
          (if (fetch/loading? (:ui/fetch-state all-links))
            (dom/div nil "Loading..."))
          (if (sequential? all-links)
            (map ui-link all-links)))))))

(defonce app
  (atom (fulcro/new-fulcro-client
          :networking (graphql-network "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0")
          :started-callback (fn [app] #_(fetch/load app :link/all-links UiLink)))))

(defn init [] (swap! app fulcro/mount Root "app-container"))

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(css/upsert-css "hackernews" Root)
