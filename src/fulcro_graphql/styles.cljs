(ns fulcro-graphql.styles)

(def color-white "#fff")
(def color-black "#000")
(def color-grey-828282 "#828282")
(def color-grey-f6f6ef "#f6f6ef")
(def color-orange-ff6600 "#ff6600")

(def font-10 "10pt")
(def font-8 "8pt")

(def container {:margin "0 auto"
                :width "85%"})

(def content-background {:background color-grey-f6f6ef})

(def flex-row {:display "flex"})
(def flex-expand {:flex "1"})

(def flex-classes
  [[:.flex flex-expand]
   [:.flex-row flex-row]
   [:.flex-align-center {:align-items "center"}]])

(def global-styles
  (concat
    flex-classes

    [[:body {:background  color-white
             :font-family "Verdana, Geneva, sans-serif"
             :font-size   font-10}]
     [:a {:color color-black
          :text-decoration "none"}]
     [:a:hover {:text-decoration "underline"}]

     [:.container container]
     [:.content-background content-background]]))
