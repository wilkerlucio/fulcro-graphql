(ns fulcro-graphql.main
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [fulcro.client.core :as fulcro]
            [fulcro.client.network :as fulcro.network]
            [cljs.core.async :refer [<! >! put!]]
            [cljs-promises.async :refer-macros [<?]]
            [goog.string :as gstr]
            [goog.object :as gobj]
            [om.next :as om]
            [om.dom :as dom]
            [clojure.string :as str]
            [pathom.core :as p]))

(cljs-promises.async/extend-promises-as-pair-channels!)

(defn js-name [s]
  (gstr/toCamelCase (name s)))

(defn pad-depth [depth]
  (str/join (repeat depth "  ")))

(defn has-call? [children]
  (boolean (first (filter (fn [{:keys [type dispatch-key]}]
                            (or (= :call type)
                                (and (= :join type)
                                     (symbol? dispatch-key)))) children))))

(defn node->graphql [{:keys [type children dispatch-key depth params]
                      :or   {depth 0}}]
  (case type
    :root
    (str (if (has-call? children) "mutation " "")
         "{\n" (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children)) "}\n")

    :join
    (cond
      (keyword? dispatch-key)
      (str (pad-depth depth) (js-name dispatch-key) " {\n"
           (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children))
           (pad-depth depth) "}\n")

      (symbol? dispatch-key)
      (str (pad-depth depth) (js-name dispatch-key) "(\n"
           (str/join ",\n" (for [[k v] params]
                             (str (pad-depth (inc depth))
                                  (js-name k) ": " (js/JSON.stringify (clj->js v)))))
           ") {\n"
           (str/join (map (comp node->graphql #(assoc % :depth (inc depth))) children))
           (pad-depth depth) "}\n"))

    :call
    (str (pad-depth depth) (js-name dispatch-key) "(\n"
         (str/join ",\n" (for [[k v] params]
                           (str (pad-depth (inc depth))
                                (js-name k) ": " (js/JSON.stringify (clj->js v)))))
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
        (if (and (map? v) query)
          (p/continue (assoc env ::p/entity v))
          (if (js/Array.isArray v)
            (mapv #(p/continue (assoc env ::p/entity %)) v)
            v)))
      ::p/continue)))

(def parser (om/parser {:read p/read}))

(defn parse [env tx]
  (-> (parser
        (assoc env
          ::p/reader graphql-reader)
        tx)
      (p/read-chan-values)))

(defn query [url q]
  (go
    (let [res (-> (js/fetch url
                            #js {:method  "post"
                                 :headers #js {"content-type" "application/json"}
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

(om/defui ^:once Root
  Object
  (render [this]
    (let [{:keys []} (om/props this)]
      (dom/div nil))))

(def root (om/factory Root))

(defn init []
  (swap! app fulcro/mount Root "app-container"))

(def sample-result
  (clj->js
    {"allLinks"
     [{"id"          "cj5k85i81mejn0146f0853mde",
       "description" "The best GraphQL client",
       "url"         "http://dev.apollodata.com/"}
      {"id"          "cj5k868x170vs0183pqlxsu0g",
       "description" "The coolest GraphQL backend 😎",
       "url"         "https://graph.cool"}
      {"id"          "cj5k8aa0emhs40146j5v4jjyg",
       "description" "The coolest GraphQL backend 😎",
       "url"         "https://graph.cool"}
      {"id"          "cj5k8bwvx78wf018371omt9zo",
       "description" "The coolest GraphQL backend 😎",
       "url"         "https://graph.cool"}
      {"id"          "cj5k8ed06at2r0134ijxcjak4",
       "description" "The coolest GraphQL backend 😎",
       "url"         "https://graph.cool"}
      {"id"          "cj5k8hvs7msi601461jqefa1t",
       "description" "The coolest GraphQL backend 😎",
       "url"         "https://graph.cool"}
      {"id"          "cj5k8tpcen6ej0146xjtkxlqr",
       "description" "Created from Om.next transaction",
       "url"         "http://www.site.com"}]}))

(comment
  (go
    (->> (query "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                [{:link/all-links [:link/id :link/description :link/url]}])
         <! js/console.log))

  sample-result

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
