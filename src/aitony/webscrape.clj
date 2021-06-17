(ns aitony.webscrape
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.core.async :refer [go]]))

(def olav "/wiki/Olav_V_of_Norway")

(def sverre "/wiki/Sverre_of_Norway")

(def henry8 "/wiki/Henry_VIII")

(def problem "/wiki/Dedo_I,_Count_of_Wettin")
(def prob2 " https://en.wikipedia.org/wiki/Jutta_of_Merseburg")

(def alfred "/wiki/Alfred_the_Great")
(def charle "/wiki/Charlemagne")
(def lionheart "/wiki/Richard_I_of_England")

(defn htmlresource [url]
  (let [full-url (str "https://en.wikipedia.org" url)]
    (try
      (html/html-resource (java.net.URL. full-url))
      (catch Exception e
        nil))))

;; (htmlresource prob2)

(defn filtersnips [k selection]
  (first (:content (second (first (filter #(= k (first (:content (first %)))) selection))))))


(defn websnippet [res]
  (html/select res {[:th] [:td]}))

(defn get-name [webresource]
  (let [namestuff (first (:content (first (html/select webresource [:th.infobox-above]))))]
    (cond
      (= java.lang.String (type namestuff)) namestuff
      (= clojure.lang.PersistentStructMap (type namestuff)) (first (:content namestuff))
      :else nil)))


(defn myscraper [myurl]
  (let [myresource (htmlresource myurl)
        myselection (websnippet myresource)
        name (get-name myresource)
        born (filtersnips "Born" myselection)
        died (filtersnips "Died" myselection)
        snipdad (filtersnips "Father" myselection)
        snipmum (filtersnips "Mother" myselection)]
    (if (nil? name)
      nil
      {:name name :born (if (= java.lang.String (type born)) born nil) :died (if (= java.lang.String (type died)) died nil)
       :father (first (:content snipdad)) :fatherref (get-in snipdad [:attrs :href])
       :mother (first (:content snipmum)) :motherref (get-in snipmum [:attrs :href])})))



(def index (atom 0))

(defn myconj [coll exclude a]
  (if (or (nil? a) (contains? exclude (hash a)))
    coll
    (conj coll {:id (swap! index inc) :url a})))

(defn looper [myurl]
  (loop [urls [{:id 0 :url myurl}]
         done #{}
         res []]
    (if-let [u (first urls)]
      (let [scrape (myscraper (:url u))]
        (if (nil? scrape)
          (recur (rest urls) (conj done (hash u)) res)
          (do
            (println (:name scrape))
            (recur (-> (rest urls)
                       (myconj done (:fatherref scrape))
                       (myconj done (:motherref scrape)))
                   (conj done (hash (:url u)))
                   (conj res (assoc (dissoc scrape :father :fatherref :mother :motherref) :id (:id u)))))))
      res)))

(looper alfred)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
