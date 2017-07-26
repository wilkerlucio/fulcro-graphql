(ns fulcro-graphql.main
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [fulcro.client.core :as fulcro]
            [cljs.core.async :refer [<! >! put!]]
            [cljs-promises.async :refer-macros [<?]]
            [goog.string :as gstr]
            [om.next :as om]
            [clojure.string :as str]))

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

(defn query [q]
  (go
    (let [res (-> (js/fetch "https://api.graph.cool/simple/v1/cj5k0e0j74cpv0122vmzoqzi0"
                            #js {:method  "post"
                                 :headers #js {"content-type" "application/json"}
                                 :body    (js/JSON.stringify #js {:query (query->graphql q)})})
                  <? .json <?)]
      (if (.-error res)
        (throw (ex-info (.-error res) {:query q}))
        (.-data res)))))

(comment
  (go
    (js/console.log (<! (query [{:link/all-links [:link/id :link/description :link/url]}]))))

  (println (query->graphql `[(link/create-link {:link/description "Created from Om.next transaction"
                                                :link/url         "http://www.site.com"})]))

  (println (query->graphql `[{(link/create-link {:link/description "Created from Om.next transaction"
                                                 :link/url         "http://www.site.com"})
                              [:link/id :link/description]}]))

  (go
    (-> (query `[(link/create-link {:link/description "Created from Om.next transaction"
                                    :link/url "http://www.site.com"})])
        <! js/console.log))

  (println (query->graphql [{:post/all-posts [:post/id :post/description :post/image-url]}])))
