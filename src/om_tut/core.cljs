(ns om-tut.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [clojure.data :as data]
            [clojure.string :as string]))

(enable-console-print!)

(def app-state
  (atom
    {:contacts
     [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
      {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
      {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
      {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
      {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
      {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]}))

(defn middle-name [{:keys [middle middle-initial]}]
  (cond
    middle (str " " middle)
    middle-initial (str " " middle-initial ".")
  ))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (count parts) 2)
      (cond-> {:first first :last last}
        (== c 1) (assoc :middle-initial middle)
        (>= c 2) (assoc :middle middle)))))

;; (parse-contact "Gerald J. Sussman")

(defn sort-by-last-name [app _]
  (om/transact! app :contacts #(vec (sort (fn [{a :last}, {b :last}] #_(js/console.log a b) (< (string/lower-case a) (string/lower-case b))) %))))

(defn add-contact [app owner]
  (let [new-contact (-> (om/get-node owner "new-contact")
                        .-value
                        parse-contact)]
    (when new-contact
      (do
        (om/transact! app :contacts #(conj % new-contact))
        (set! (.-value (om/get-node owner "new-contact")) nil) ; reset the input
        ))))

(defn contact-view [contact owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/li nil
        (dom/span nil (display-name contact))
        (dom/button #js {:onClick (fn [e] (put! delete @contact))} "Delete")))))

(defn contacts-view [app owner]
  (reify
    om/IInitState
      (init-state [_]
        {:delete (chan)})
    om/IWillMount
      (will-mount [_]
        (let [delete (om/get-state owner :delete)]
          (go (loop []
            (let [contact (<! delete)]
              (om/transact! app :contacts
                (fn [xs] (vec (remove #(= contact %) xs))))
              (recur))))))
    om/IRenderState
    (render-state [this {:keys [delete]}]
      (dom/div nil
        (dom/h2 nil "Contact list")
        (dom/button #js {:onClick #(sort-by-last-name app owner)}
          "Sort by last name")
        (apply dom/ul nil
          (om/build-all contact-view (:contacts app)
            {:init-state {:delete delete}}))
        (dom/div nil
          (dom/input #js {:type "text" :ref "new-contact"})
          (dom/button #js {:onClick #(add-contact app owner)} "Add contact"))))))

(om/root contacts-view app-state
  {:target (. js/document (getElementById "contacts"))})
