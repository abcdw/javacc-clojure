(ns compojure.core
  "A DSL for building Ring handlers from smaller routes.

  Compojure routes are semantically the same as Ring handlers, with the
  exception that routes may return `nil` to indicate they do not match.

  This namespace provides functions and macros for concisely constructing
  routes and combining them together to form more complex functions."
  (:require [compojure.response :as response]
            [clojure.tools.macro :as macro]
            [clout.core :as clout]
            [ring.middleware.head :refer [wrap-head]]
            [ring.util.codec :as codec]
            [medley.core :refer [map-vals]]))

(defn- form-method [request]
  (or (get-in request [:form-params "_method"])
      (get-in request [:multipart-params "_method"])))

(defn- method-matches? [request method]
  (or (nil? method)
      (let [request-method (:request-method request)]
        (case request-method
          :head
          (or (= method :head) (= method :get))
          :post
          (if-let [fm (form-method request)]
            (.equalsIgnoreCase (name method) fm)
            (= method :post))
          ;else
          (= request-method method)))))

(defn- head-response [response request method]
  (if (and response (= :get method) (= :head (:request-method request)))
    (assoc response :body nil)
    response))

(defn- decode-route-params [params]
  (map-vals codec/url-decode params))

(defn- assoc-route-params [request params]
  (merge-with merge request {:route-params params, :params params}))

(defn- route-matches [route request]
  (let [path (:compojure/path request)]
    (clout/route-matches route (cond-> request path (assoc :path-info path)))))

(defn- route-request [request route]
  (if-let [params (route-matches route request)]
    (assoc-route-params request (decode-route-params params))))

(defn- literal? [x]
  (if (coll? x)
    (every? literal? x)
    (not (or (symbol? x) (list? x)))))

(defn- prepare-route [route]
  (cond
    (string? route)
      (clout/route-compile route)
    (and (vector? route) (literal? route))
      (clout/route-compile
       (first route)
       (apply hash-map (rest route)))
    (vector? route)
      `(clout/route-compile
        ~(first route)
        ~(apply hash-map (rest route)))
    :else
      `(if (string? ~route)
         (clout/route-compile ~route)
         ~route)))

(defn- and-binding [req binds]
  `(dissoc (:params ~req) ~@(map keyword (keys binds)) ~@(map str (keys binds))))

(defn- symbol-binding [req sym]
  `(get-in ~req [:params ~(keyword sym)] (get-in ~req [:params ~(str sym)])))

(defn- application-binding [req sym func]
  `(~func ~(symbol-binding req sym)))

(defn- vector-bindings [args req]
  (loop [args args, binds {}]
    (if (seq args)
      (let [[x y z] args]
        (cond
          (= '& x)
          (recur (nnext args) (assoc binds y (and-binding req binds)))
          (= :as x)
          (recur (nnext args) (assoc binds y req))
          (and (symbol? x) (= :<< y) (nnext args))
          (recur (drop 3 args) (assoc binds x (application-binding req x z)))
          (symbol? x)
          (recur (next args) (assoc binds x (symbol-binding req x)))
          :else
          (throw (Exception. (str "Unexpected binding: " x)))))
      (mapcat identity binds))))

(defn- warn-on-*-bindings! [bindings]
  (when (and (vector? bindings) (contains? (set bindings) '*))
    (binding [*out* *err*]
      (println "WARNING: * should not be used as a route binding."))))

(defn- application-symbols [args]
  (loop [args args, syms '()]
    (if (seq args)
      (let [[x y] args]
        (if (and (symbol? x) (= :<< y))
          (recur (drop 3 args) (conj syms x))
          (recur (next args) syms)))
      (seq syms))))

(defmacro ^:no-doc let-request [[bindings request] & body]
  (warn-on-*-bindings! bindings)
  (if (vector? bindings)
    `(let [~@(vector-bindings bindings request)]
       ~(if-let [syms (application-symbols bindings)]
          `(if (and ~@(for [s syms] `(not (nil? ~s)))) (do ~@body))
          `(do ~@body)))
    `(let [~bindings ~request] ~@body)))

(defn- wrap-route-middleware [handler]
  (fn
    ([request]
     (if-let [mw (:route-middleware request)]
       ((mw handler) request)
       (handler request)))
    ([request respond raise]
     (if-let [mw (:route-middleware request)]
       ((mw handler) request respond raise)
       (handler request respond raise)))))

(defn- wrap-route-info [handler route-info]
  (fn
    ([request]
     (handler (assoc request :compojure/route route-info)))
    ([request respond raise]
     (handler (assoc request :compojure/route route-info) respond raise))))

(defn- wrap-route-matches [handler method path]
  (fn
    ([request]
     (if (method-matches? request method)
       (if-let [request (route-request request path)]
         (-> (handler request)
             (head-response request method)))))
    ([request respond raise]
     (if (method-matches? request method)
       (if-let [request (route-request request path)]
         (handler request #(respond (head-response % request method)) raise)
         (respond nil))
       (respond nil)))))

(defn- wrap-response [handler]
  (fn
    ([request]
     (response/render (handler request) request))
    ([request respond raise]
     (response/send (handler request) request respond raise))))

(defn make-route
  "Returns a function that will only call the handler if the method and path
  match the request."
  [method path handler]
  (-> handler
      (wrap-response)
      (wrap-route-middleware)
      (wrap-route-info [(or method :any) (str path)])
      (wrap-route-matches method path)))

(defn compile-route
  "Compile a route in the form `(method path bindings & body)` into a function.
  Used to create custom route macros."
  [method path bindings body]
  `(make-route
    ~method
    ~(prepare-route path)
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defn routing
  "Apply a list of routes to a Ring request map."
  [request & handlers]
  (some #(% request) handlers))

(defn routes
  "Create a Ring handler by combining several handlers into one."
  [& handlers]
  (fn
    ([request]
     (apply routing request handlers))
    ([request respond raise]
     (letfn [(f [handlers]
               (if (seq handlers)
                 (let [handler  (first handlers)
                       respond' #(if % (respond %) (f (rest handlers)))]
                   (handler request respond' raise))
                 (respond nil)))]
       (f handlers)))))

(defmacro defroutes
  "Define a Ring handler function from a sequence of routes. The name may
  optionally be followed by a doc-string and metadata map."
  [name & routes]
  (let [[name routes] (macro/name-with-attributes name routes)]
   `(def ~name (routes ~@routes))))

(defmacro GET "Generate a `GET` route."
  [path args & body]
  (compile-route :get path args body))

(defmacro POST "Generate a `POST` route."
  [path args & body]
  (compile-route :post path args body))

(defmacro PUT "Generate a `PUT` route."
  [path args & body]
  (compile-route :put path args body))

(defmacro DELETE "Generate a `DELETE` route."
  [path args & body]
  (compile-route :delete path args body))

(defmacro HEAD "Generate a `HEAD` route."
  [path args & body]
  (compile-route :head path args body))

(defmacro OPTIONS "Generate an `OPTIONS` route."
  [path args & body]
  (compile-route :options path args body))

(defmacro PATCH "Generate a `PATCH` route."
  [path args & body]
  (compile-route :patch path args body))

(defmacro ANY "Generate a route that matches any method."
  [path args & body]
  (compile-route nil path args body))

(defn ^:no-doc make-rfn [handler]
  (-> handler
      wrap-response
      wrap-route-middleware
      wrap-head))

(defmacro rfn "Generate a route that matches any method and path."
  [args & body]
  `(make-rfn (fn [request#] (let-request [~args request#] ~@body))))

(defn- remove-suffix [path suffix]
  (subs path 0 (- (count path) (count suffix))))

(defn- context-request [request route]
  (if-let [params (clout/route-matches route request)]
    (let [uri     (:uri request)
          path    (:path-info request uri)
          context (or (:context request) "")
          subpath (:__path-info params)
          params  (dissoc params :__path-info)]
      (-> request
          (assoc-route-params (decode-route-params params))
          (assoc :path-info (if (= subpath "") "/" subpath)
                 :context   (remove-suffix uri subpath))))))

(defn- context-route [route]
  (let [re-context {:__path-info #"|/.*"}]
    (cond
      (string? route)
        (clout/route-compile (str route ":__path-info") re-context)
      (and (vector? route) (literal? route))
        (clout/route-compile
         (str (first route) ":__path-info")
         (merge (apply hash-map (rest route)) re-context))
      (vector? route)
       `(clout/route-compile
         (str ~(first route) ":__path-info")
         ~(merge (apply hash-map (rest route)) re-context))
      :else
       `(clout/route-compile (str ~route ":__path-info") ~re-context))))

(defn ^:no-doc make-context [route make-handler]
  (letfn [(handler
            ([request]
             ((make-handler request) request))
            ([request respond raise]
             ((make-handler request) request respond raise)))]
    (if (#{":__path-info" "/:__path-info"} (:source route))
      handler
      (fn
        ([request]
         (if-let [request (context-request request route)]
           (handler request)))
        ([request respond raise]
         (if-let [request (context-request request route)]
           (handler request respond raise)
           (respond nil)))))))

(defmacro context
  "Give all routes in the form a common path prefix and set of bindings.

  The following example demonstrates defining two routes with a common
  path prefix ('/user/:id') and a common binding ('id'):

      (context \"/user/:id\" [id]
        (GET \"/profile\" [] ...)
        (GET \"/settings\" [] ...))"
  [path args & routes]
  `(make-context
    ~(context-route path)
    (fn [request#]
      (let-request [~args request#]
        (routes ~@routes)))))

(defmacro let-routes
  "Takes a vector of bindings and a body of routes.

  Equivalent to:

      (let [...] (routes ...))"
  [bindings & body]
  `(let ~bindings (routes ~@body)))

(defn- pre-init [middleware]
  (let [proxy (middleware
               (fn
                 ([request]
                  ((:route-handler request) request))
                 ([request respond raise]
                  ((:route-handler request) request respond raise))))]
    (fn [handler]
      (let [prep-request #(assoc % :route-handler handler)]
        (fn
          ([request]
           (proxy (prep-request request)))
          ([request respond raise]
           (proxy (prep-request request) respond raise)))))))

(defn wrap-routes
  "Apply a middleware function to routes after they have been matched."
  ([handler middleware]
   (let [middleware   (pre-init middleware)
         prep-request (fn [request]
                        (let [mw (:route-middleware request identity)]
                          (assoc request :route-middleware (comp mw middleware))))]
       (fn
         ([request]
          (handler (prep-request request)))
         ([request respond raise]
          (handler (prep-request request) respond raise)))))
  ([handler middleware & args]
     (wrap-routes handler #(apply middleware % args))))
(ns compojure.route
  "Functions for defining common types of routes."
  (:require [compojure.response :as response]
            [compojure.core :refer [GET rfn]]
            [ring.util.mime-type :as mime]
            [ring.util.response :refer [file-response resource-response
                                        status content-type]]))

(defn- add-wildcard [^String path]
  (str path (if (.endsWith path "/") "*" "/*")))

(defn- add-mime-type [response path options]
  (if-let [mime-type (mime/ext-mime-type path (:mime-types options {}))]
    (content-type response mime-type)
    response))

(defn files
  "Returns a route for serving static files from a directory.
  Accepts the following options:
  :root
  : the root path where the files are stored, defaults to \"public\"
  :mime-types
  : an optional map of file extensions to mime types"
  ([path]
   (files path {}))
  ([path options]
   (GET (add-wildcard path) {{file-path :*} :route-params}
     (let [options  (merge {:root "public"} options)
           response (file-response file-path options)]
       (if response
         (add-mime-type response (str (:body response)) options))))))

(defn resources
  "Returns a route for serving resources on the classpath.
  Accepts the following options:
  :root
  : the root prefix path of the resources, defaults to \"public\"
  :mime-types
  : an optional map of file extensions to mime types"
  ([path]
   (resources path {}))
  ([path options]
   (GET (add-wildcard path) {{resource-path :*} :route-params}
     (let [root (:root options "public")]
       (some-> (resource-response (str root "/" resource-path))
               (add-mime-type resource-path options))))))

(defn not-found
  "Returns a route that always returns a 404 \"Not Found\" response with the
  supplied response body. The response body may be anything accepted by the
  [[response/render]] function."
  [body]
  (fn handler
    ([request]
     (-> (response/render body request)
         (status 404)
         (cond-> (= (:request-method request) :head) (assoc :body nil))))
    ([request respond raise]
     (respond (handler request)))))

(ns compojure.response
  "A protocol for generating Ring response maps"
  (:refer-clojure :exclude [send])
  (:require [ring.util.mime-type :as mime]
            [ring.util.response :as response]))

(defprotocol Renderable
  "A protocol that tells Compojure how to handle the return value of routes
  defined by [[GET]], [[POST]], etc.
  This protocol supports rendering strings, maps, functions, refs, files, seqs,
  input streams and URLs by default, and may be extended to cover many custom
  types."
  (render [x request]
    "Render `x` into a form suitable for the given request map."))

(defprotocol Sendable
  "A protocol that tells Compojure how to handle the return value of
  asynchronous routes, should they require special attention."
  (send* [x request respond raise]))

(defn send
  "Send `x` as a Ring response. Checks to see if `x` satisfies [[Sendable]],
  and if not, falls back to [[Renderable]]."
  [x request respond raise]
  (if (satisfies? Sendable x)
    (send* x request respond raise)
    (respond (render x request))))

(defn- guess-content-type [response name]
  (if-let [mime-type (mime/ext-mime-type (str name))]
    (response/content-type response mime-type)
    response))

(extend-protocol Renderable
  nil
  (render [_ _] nil)
  String
  (render [body _]
    (-> (response/response body)
        (response/content-type "text/html; charset=utf-8")))
  clojure.lang.APersistentMap
  (render [resp-map _]
    (merge (with-meta (response/response "") (meta resp-map))
           resp-map))
  clojure.lang.Fn
  (render [func request] (render (func request) request))
  clojure.lang.MultiFn
  (render [func request] (render (func request) request))
  clojure.lang.IDeref
  (render [ref request] (render (deref ref) request))
  java.io.File
  (render [file _]
    (-> (response/file-response (str file))
        (guess-content-type file)))
  clojure.lang.ISeq
  (render [coll _]
    (-> (response/response coll)
        (response/content-type "text/html; charset=utf-8")))
  java.io.InputStream
  (render [stream _] (response/response stream))
  java.net.URL
  (render [url _]
    (-> (response/url-response url)
        (guess-content-type url))))

(extend-protocol Sendable
  clojure.lang.Fn
  (send* [func request respond raise]
    (func request #(send % request respond raise) raise))
  clojure.lang.MultiFn
  (send* [func request respond raise]
    (func request #(send % request respond raise) raise)))

(ns compojure.middleware
  "Optional middleware to enhance routing in Compojure."
  (:require [compojure.core :refer [wrap-routes]]
            [ring.util.response :as resp]))

(defn remove-trailing-slash
  "Remove the trailing '/' from a URI string, if it exists."
  [^String uri]
  (if (.endsWith uri "/")
    (.substring uri 0 (dec (.length uri)))
    uri))

(defn- redirect-to-canonical
  ([request]
   (resp/redirect (:compojure/path request) 301))
  ([request respond raise]
   (respond (redirect-to-canonical request))))

(defn- assoc-path [request path]
  (assoc request :compojure/path path))

(defn wrap-canonical-redirect
  "Middleware that permanently redirects any non-canonical route to its
  canonical equivalent, based on a make-canonical function that changes a URI
  string into its canonical form. If not supplied, the make-canonical function
  will default to [[remove-trailing-slash]]."
  ([handler]
   (wrap-canonical-redirect handler remove-trailing-slash))
  ([handler make-canonical]
   (let [redirect-handler (wrap-routes handler (constantly redirect-to-canonical))]
     (fn
       ([{uri :uri :as request}]
        (let [canonical-uri (make-canonical uri)]
          (if (= uri canonical-uri)
            (handler request)
            (redirect-handler (assoc-path request canonical-uri)))))
       ([{uri :uri :as request} respond raise]
        (let [canonical-uri (make-canonical uri)]
          (if (= uri canonical-uri)
            (handler request respond raise)
            (redirect-handler (assoc-path request canonical-uri) respond raise))))))))

(ns compojure.handler
  "Functions to create Ring handlers from routes.

  This namespace has been **DEPRECATED** in favor of the [ring-defaults][]
  library.

  [ring-defaults]: https://github.com/ring-clojure/ring-defaults"
  {:deprecated "1.2"}
  (:use [ring.middleware params
                         keyword-params
                         nested-params
                         multipart-params
                         cookies
                         session
                         flash]))

(defn- with-opts [routes middleware opts]
  (if opts
    (middleware routes opts)
    (middleware routes)))

(defn api
  "Create a handler suitable for a web API. This adds the following
  middleware to your routes:

  - wrap-params
  - wrap-nested-params
  - wrap-keyword-params"
  {:deprecated "1.2"}
  [routes]
  (-> routes
      wrap-keyword-params
      wrap-nested-params
      wrap-params))

(defn site
  "Create a handler suitable for a standard website. This adds the
  following middleware to your routes:

  - wrap-session
  - wrap-flash
  - wrap-cookies
  - wrap-multipart-params
  - wrap-params
  - wrap-nested-params
  - wrap-keyword-params

  A map of options may also be provided. These keys are provided:

  :session
  : a map of session middleware options

  :multipart
  : a map of multipart-params middleware options"
  {:deprecated "1.2"
   :arglists '([routes] [routes options])}
  [routes & [opts]]
  (-> (api routes)
      (with-opts wrap-multipart-params (:multipart opts))
      (wrap-flash)
      (with-opts wrap-session (:session opts))))
