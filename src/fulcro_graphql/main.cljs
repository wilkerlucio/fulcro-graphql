(ns fulcro-graphql.main
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs-promises.async :refer-macros [<?]]
            [cljs.core.async :refer [<! >! put! promise-chan]]
            [cljs.spec.alpha :as s]
            [clojure.string :as str]
            [fulcro.client.core :as fulcro]
            [fulcro.client.data-fetch :as fetch]
            [fulcro.client.network :as fulcro.network]
            [goog.events :as events]
            [goog.object :as gobj]
            [goog.string :as gstr]
            [om.dom :as dom]
            [om.next :as om]
            [pathom.core :as p]
            [spec-coerce.core :as sc])
  (:import [goog.net XhrIo EventType]))

(cljs-promises.async/extend-promises-as-pair-channels!)

(defn get-load-query [comp]
  (conj (om/get-query comp) :ui/fetch-state))

(defn js-name [s]
  (gstr/toCamelCase (name s)))

(defn pad-depth [depth]
  (str/join (repeat depth "  ")))

(defn has-call? [children]
  (boolean (first (filter (fn [{:keys [type dispatch-key]}]
                            (or (= :call type)
                                (and (= :join type)
                                     (symbol? dispatch-key)))) children))))

(defn stringify [x] (js/JSON.stringify (clj->js x)))

(defn params->graphql
  ([x] (params->graphql x true))
  ([x root?]
   (cond
     (map? x)
     (let [params (->> (into [] (map (fn [[k v]] (str (js-name k) ": " (params->graphql v false)))) x)
                       (str/join ", "))]
       (if root?
         (str "(" params ")")
         (str "{" params "}")))

     :else
     (stringify x))))

(defn node->graphql [{:keys [type children dispatch-key depth params]
                      :or   {depth 0}}]
  (case type
    :root
    (str (if (has-call? children) "mutation " "")
         "{\n" (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children)) "}\n")

    :join
    (cond
      (keyword? dispatch-key)
      (str (pad-depth depth) (js-name dispatch-key) (some-> params params->graphql) " {\n"
           (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children))
           (pad-depth depth) "}\n")

      (symbol? dispatch-key)
      (str (pad-depth depth) (js-name dispatch-key) "(\n"
           (str/join ",\n" (for [[k v] params]
                             (str (pad-depth (inc depth))
                                  (js-name k) ": " (stringify v))))
           ") {\n"
           (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children))
           (pad-depth depth) "}\n"))

    :call
    (str (pad-depth depth) (js-name dispatch-key) "(\n"
         (->> (for [[k v] params]
                (str (pad-depth (inc depth))
                     (js-name k) ": " (stringify v)))
              (str/join ",\n"))
         ") {id}\n")

    :prop
    (str (pad-depth depth)
         (js-name dispatch-key) "\n")))

(defn query->graphql [query]
  (node->graphql (om/query->ast query)))

(defn graphql-reader [{:keys    [query ast]
                       ::p/keys [entity]
                       :as      env}]
  (let [js-key (js-name (:key ast))]
    (if (gobj/containsKey entity js-key)
      (let [v (gobj/get entity js-key)]
        (if (js/Array.isArray v)
          (mapv #(p/continue (assoc env ::p/entity %)) v)
          (if (and query (= (type v) js/Object))
            (p/continue (assoc env ::p/entity v))
            (sc/coerce (:key ast) v))))
      ::p/continue)))

(def parser (om/parser {:read p/read}))

(defn parse [env tx]
  (-> (parser
        (assoc env
          ::p/reader graphql-reader)
        tx)
      (p/read-chan-values)))

(defn http [{::keys [url body method headers]
             :or {method "GET"}}]
  (let [c (promise-chan)
        xhr (XhrIo.)]
    (events/listen xhr (.-SUCCESS EventType) #(put! c [% nil]))
    (events/listen xhr (.-ERROR EventType) #(put! c [nil %]))
    (.send xhr url method body (clj->js headers))))

(defn query [{::keys [url q]}]
  (go
    (let [res (-> (http #::{:url     url
                            :method  "post"
                            :headers {"content-type" "application/json"}
                            :body    (js/JSON.stringify #js {:query (query->graphql q)})})
                  <? .json <?)]
      (if (.-error res)
        (throw (ex-info (.-error res) {:query q}))
        (<! (parse {::p/entity (.-data res)} q))))))

(defrecord Network [url completed-app]
  fulcro.network/NetworkBehavior
  (serialize-requests? [_] true)

  fulcro.network/FulcroNetwork
  (send [_ edn ok error]
    (go
      (try
        (let [res (-> (js/fetch url
                                #js {:method  "post"
                                     :headers #js {"content-type" "application/json"}
                                     :body    (js/JSON.stringify #js {:query (query->graphql edn)})})
                      <? .json <?)]
          (if (.-error res)
            (error (ex-info (.-error res) {:query edn}))
            (ok (<! (parse {::p/entity (.-data res)} edn)))))
        (catch :default e (error e)))))

  (start [this app] (assoc this :complete-app app)))

(defn make-network [url options]
  (map->Network {:url url}))

(defonce app
  (atom (fulcro/new-fulcro-client :networking (make-network "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0" {}))))

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

(def root (om/factory Root))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(init)

(defn log-state []
  (->> @app :reconciler :config :state deref
       js/console.log))

(s/def :link/updated-at inst?)

(comment
  (go
    (->> (query "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                [{:link/all-links [:link/id :link/description :link/url :link/updated-at]}])
         <! js/console.log))

  (go
    (->> (query "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                ['()])
         <! js/console.log))

  (go
    (->> (query "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                [{:all-links [:id :description :url]}])
         <! js/console.log))

  (println (query->graphql [{:all-links [:id :description :url]}]))

  (go
    (->> (query "https://api.github.com/graphql?access_token="
                `[{:viewer
                   [:login
                    ({:repositories
                      [{:nodes [:name :description]}]}
                      {:last 10})]}
                  ({:repository [:id :description]}
                    {:name "spec-coerce" :owner "wilkerlucio"})])
         <! js/console.log))

  (println (query->graphql `[{:viewer
                              [:login
                               ({:repositories
                                 [{:nodes [:name :description]}]}
                                 {:last 10})]}
                             ({:repository [:id :description]}
                               {:name "spec-coerce" :owner "wilkerlucio"})]))

  (params->graphql {:name "spec-coerce" :owner "wilkerlucio"})

  (js/console.log (js/Array.isArray #js []))

  (go
    (->> (parse {::p/entity sample-result} [{:link/all-links [:link/id :link/description :link/url]}])
         <! js/console.log))

  (println (query->graphql `[(link/create-link {:link/description "Created from Om.next transaction"
                                                :link/url         "http://www.site.com"})]))

  (println (query->graphql `[{(link/create-link {:link/description "Created from Om.next transaction"
                                                 :link/url         "http://www.site.com"})
                              [:link/id :link/description]}]))

  (go
    (-> (query `[(link/create-link {:link/description "Created from Om.next transaction"
                                    :link/url         "http://www.site.com"})])
        <! js/console.log))

  (println (query->graphql [{:post/all-posts [:post/id :post/description :post/image-url]}])))
