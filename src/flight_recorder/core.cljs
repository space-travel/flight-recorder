(ns ^:figwheel-hooks flight-recorder.core
  (:require
   [cljsjs.twemoji]
   [clojure.string :as str]
   [goog.dom :as gdom]
   [goog.object :as gobj]
   [om.next :as om]
   [om.dom :as dom]
   [hom.core :refer-macros [component defui]]))

(extend-type js/Float32Array
  ISeqable
  (-seq [array] (array-seq array 0)))

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:tracks []}))

(defmulti read om/dispatch)

(defmethod read :tracks
  [{:keys [state query]} key _]
  (let [st @state]
    {:value (om/db->tree query (get st key) st)}))

(defmethod read :devices
  [{:keys [state ast]} key {:keys [enumerate?]}]
  (merge
    {:value (get @state key)}
    (when enumerate?
      {:enumerate-devices ast})))

(defmulti mutate om/dispatch)

(defmethod mutate 'tracks/add
  [{:keys [state]} _ _]
  {:value {:keys [:tracks]}
   :action
   (fn []
     (let [ref [:track/by-id (random-uuid)]
           track {:id (second ref) :name "" :device "default"}]
       (swap! state (fn [st]
                      (-> st
                          (assoc-in ref track)
                          (update :tracks #(into [] (conj % ref))))))))})

(defmethod mutate 'tracks/update
  [{:keys [state]} _ {:keys [ref data]}]
  {:action
   (fn []
     (swap! state update-in ref #(merge % data)))})

(defmethod mutate 'tracks/remove
  [{:keys [state]} _ {:keys [ref]}]
  {:value {:keys [:tracks]}
   :action
   (fn []
     (swap! state (fn [st]
                    (-> st
                        (update :tracks #(into [] (remove #{%2} %1)) ref)
                        (update :track/by-id dissoc (second ref))))))})

(defn emojify [s]
  (.parse js/twemoji s))

(defn update-track! [cmp data]
  (om/transact! cmp
    `[(tracks/update {:ref ~(om/get-ident cmp) :data ~data})]))

(defn update-track-name! [cmp]
  (let [name (dom/node (om/react-ref cmp "name"))]
    (update-track! cmp {:name (gobj/get name "value")})))

(defn update-track-device! [cmp]
  (let [device (dom/node (om/react-ref cmp "device"))]
    (update-track! cmp {:device (gobj/get device "value")})))

(defn open-device [device]
  (let [constraints (clj->js {:audio {:deviceId {:exact device}}})]
    (js/navigator.mediaDevices.getUserMedia constraints)))

(defn connect-device [audio-context output-node device]
  (.then (open-device device)
         (fn [stream]
           (let [gain (.createGain audio-context)
                 analyser (doto (.createAnalyser audio-context)
                            (gobj/set "fftSize" 1024))
                 compressor (.createDynamicsCompressor audio-context)
                 limiter (.createDynamicsCompressor audio-context)
                 input (.createMediaStreamSource audio-context stream)]
             (.setValueAtTime (.-gain gain) 1.0 (.-currentTime audio-context))
             (.setValueAtTime (.-threshold compressor) -14 (.-currentTime audio-context))
             (.setValueAtTime (.-knee compressor) 0.9 (.-currentTime audio-context))
             (.setValueAtTime (.-ratio compressor) 4 (.-currentTime audio-context))
             (.setValueAtTime (.-attack compressor) 0.035 (.-currentTime audio-context))
             (.setValueAtTime (.-release compressor) 0.050 (.-currentTime audio-context))
             (.setValueAtTime (.-threshold limiter) 0 (.-currentTime audio-context))
             (.setValueAtTime (.-ratio limiter) 100 (.-currentTime audio-context))
             (.connect input compressor)
             (.connect compressor gain)
             (.connect gain limiter)
             (.connect limiter analyser)
             (.connect analyser output-node)
             (.connect analyser (.-destination audio-context))
             {:input input :node analyser :analyser analyser}))))

(defui Volume
  Object
  (componentDidMount [this]
    (js/requestAnimationFrame
      (fn query []
        (when (om/mounted? this)
          (do
            (when-some [analyser (-> this om/props :analyser)]
              (let [buffer (js/Float32Array. (gobj/get analyser "fftSize"))]
                (.getFloatTimeDomainData analyser buffer)
                (om/update-state! this assoc :volume
                  (max (Math/sqrt (/ (reduce + 0 (map #(* % %) buffer)) (.-length buffer)))
                       (* (get (om/get-state this) :volume 0) 0.95)))))
            (js/requestAnimationFrame query))))))
  (render [this]
    (let [volume (get (om/get-state this) :volume 0)]
      [:section
        [:label {:dangerouslySetInnerHTML {:__html (emojify "🔈")}}]
        [:meter {:min 0 :max 1 :low 0.5 :high 0.9 :optimum 0.25 :value volume}]])))

(def volume (om/factory Volume))

(defn order [n]
  (if (zero? n)
    0
    (Math/floor (+ (/ (Math/log n) Math/LN10) 0.000000001))))

(defn ms->interval [n]
  (last (reduce (fn [[m s] d]
                  (let [v (mod m d)
                        od (order (dec d))]
                    [(quot m d)
                     (str (when (< v (Math/pow 10 od))
                            (str/join "" (repeat (- od (order v)) "0")))
                          v (when-not (str/blank? s) ":") s)]))
                [(Math/round n) nil]
                [1000 60 60 24])))

(defui Timer
  Object
  (componentDidMount [this]
    (js/requestAnimationFrame
      (fn query []
        (when (om/mounted? this)
          (do
            (when-some [start-time (-> this om/props :start-time)]
              (om/update-state! this assoc :time (- (js/performance.now) start-time)))
            (js/requestAnimationFrame query))))))
  (render [this]
    (let [t (get (om/get-state this) :time 0)]
      [:span (ms->interval t)])))

(def timer (om/factory Timer))

(defui Track
  static om/Ident
  (ident [_ {:keys [id]}]
    [:track/by-id id])
  static om/IQuery
  (query [_]
    [:id :name :device])
  Object
  (connect-device [this]
    (let [device (-> this om/props :device) 
          {:keys [audio-context output-node]} (om/shared this)]
      (.disconnect-device this)
      (.then (connect-device audio-context output-node device)
             #(om/update-state! this merge %))))
  (disconnect-device [this]
    (let [{:keys [input node]} (om/get-state this)]
      (when (and (some? input) (some? node))
        (do
          (.disconnect input)
          (.disconnect node)))))
  (componentWillMount [this]
    (.connect-device this))
  (componentWillUnmount [this]
    (.disconnect-device this))
  (componentDidUpdate [this prev-props prev-state]
    (when-not (= (:device prev-props) (-> this om/props :device))
      (.connect-device this)))
  (render [this]
    (let [{:keys [id name device]} (om/props this)
          {:keys [devices track-delete]} (om/get-computed this)]
      [:div.fr-track
       [:button {:onClick #(track-delete (om/get-ident this))} "-"]
       [:input  
        {:ref :track/name
         :type "text"
         :placeholder "new track"
         :value name
         :onChange #(update-track-name! this)}]
       [:section
        [:label {:dangerouslySetInnerHTML {:__html (emojify "🎙️")}}]
        [:select
         {:ref :track/device
          :value device
          :onChange #(update-track-device! this)}
         (map #(component
                 [:option {:key (gobj/get % "deviceId") :value (gobj/get % "deviceId")}
                 (gobj/get % "label")])
             devices)]]
       (volume (om/get-state this))])))

(def track (om/factory Track {:keyfn :id}))

; https://developers.google.com/web/updates/2017/09/autoplay-policy-changes#webaudio
(defn wrap-for-audio-context! [f cmp]
 (let [audio-context (-> cmp om/shared :audio-context)]
   (if (= (.-state audio-context) "suspended")
     (.then (.resume audio-context)
            #(f)) 
     (f))))

; NOTE this is by definition the only intial action a user can take
(defn add-track! [cmp]
  (wrap-for-audio-context! #(om/transact! cmp `[(tracks/add)]) cmp))

(defn remove-track! [cmp track]
  (om/transact! cmp `[(tracks/remove {:ref ~track})]))

(defn create-recorder [stream]
  (doto (js/MediaRecorder. stream)
    (gobj/set "ondataavailable"
      (fn [event]
        (let [url (js/URL.createObjectURL (.-data event))]
          (println url)
          (doto (js/document.createElement "a")
            (gobj/set "href" url)
            (gobj/set "download" "audio.webm")
            (.click)))))))

(defn start-recording! [cmp]
  (let [output-node (-> cmp om/shared :output-node)
        media-recorder (-> cmp om/get-state :media-recorder)]
    (om/update-state! cmp merge
      {:start-time (when-not (some? media-recorder) (js/performance.now))
       :media-recorder (if (some? media-recorder)
                         (do (.stop media-recorder))
                         (doto (create-recorder (.-stream output-node))
                           (.start)))})))

(defui App
  static om/IQuery
  (query [_]
    `[(:devices {:enumerate? true}) {:tracks ~(om/get-query Track)}])
  Object
  (render [this]
    (let [{:keys [devices tracks]} (om/props this)]
      [:article
       [:h1.f1.measure.tc
        {:dangerouslySetInnerHTML {:__html (emojify "✈️  flight recorder")}}]
       [:section.tc.pb1
        [:button
         {:onClick #(add-track! this)} "+"]
        [:button
         {:onClick #(start-recording! this)
          :disabled (empty? tracks)
          :className (when-some [media-recorder (-> this om/get-state :media-recorder)]
                       "fr-recording")
          :dangerouslySetInnerHTML {:__html (emojify "🔴")}}]
        [:button.fr-timer
         [:label {:dangerouslySetInnerHTML {:__html (emojify "⏲️")}}]
         " "
         (timer (om/get-state this))]]
       [:section.pt3
        (map #(track
                (om/computed %
                  {:devices devices
                   :track-delete (fn [ref] (remove-track! this ref))}))
             tracks)]])))

(defn enumerate-devices []
  (.then (js/navigator.mediaDevices.enumerateDevices)
         (fn [devices-info]
           (filter #(= (gobj/get % "kind") "audioinput") devices-info))))

(def ^:dynamic *audio-context* (js/AudioContext.))

(def reconciler
  (om/reconciler
    {:state app-state
     :normalize true
     :shared {:audio-context *audio-context*
              :output-node (.createMediaStreamDestination *audio-context*)}
     :parser (om/parser {:read read :mutate mutate})
     :remotes [:enumerate-devices :record]
     :send (fn [params callback]
             (.then (enumerate-devices)
                    #(callback {:devices %})))}))

(gobj/set js/navigator.mediaDevices "ondevicechange"
  (fn []
    (.then (enumerate-devices)
           #(om/merge! reconciler {:devices %}))))

(defn get-app-element []
  (gdom/getElement "app"))

(om/add-root! reconciler
  App (get-app-element))

;; specify reload hook with ^;after-load metadata
(defn ^:after-load on-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
