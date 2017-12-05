

;; You are given 2 maps which you need to color with four colors: (R G B Y)
;; The method used is "cutset conditioning"
;; Use this to color a map with as few colors as possible.


;; the possible colors for the maps
(defvar *colors* '(R G B Y))

;; first map (given in Projectf17.pdf)
(defvar *map-1* '((A (B C E))
                  (B (A E F))
                  (C (A E F))
                  (D (F))
                  (E (A B C F))
                  (F (B C D E))))

;;
;;  All 50 U.S. States and the District of Columbia.
;;
;;  State abbreviations from 
;; http://www.usps.com/ncsc/lookups/abbr_state.txt
;;
(defvar *50-states* '(
                    (AL (GA FL MS TN))             ; AL = Alabama
                    (AK ())                        ; AK = Alaska
                    (AZ (CA NV UT CO NM))          ; AZ = Arizona
                    (AR (TX OK MO TN MS LA))       ; AR = Arkansas
                    (CA (OR NV AZ))                ; CA = California
                    (CO (NM AZ UT WY NE KS OK))    ; CO = Colorado
                    (CT (RI NY MA))                ; CT = Conneticut
                    (DE (MD PA NJ))                ; DE = Delaware
                    (DC (MD VA))                   ; DC = D.C.
                    (FL (GA AL))                   ; FL = Florida
                    (GA (SC NC TN AL FL))          ; GA = Georgia
                    (HI ())                        ; HI = Hawaii
                    (ID (WA OR NV UT WY MT))       ; ID = Idaho
                    (IL (WI IA MO KY IN))          ; IL = Illinois
                    (IN (IL KY OH MI))             ; IN = Indiana
                    (IA (MN SD NE MO IL WI))       ; IA = Iowa
                    (KS (CO OK MO NE))             ; KS = Kansas
                    (KY (MO TN VA WV OH IN IL))    ; KY = Kentucky
                    (LA (TX AR MS))                ; LA = Lousiana
                    (ME (NH))                      ; ME = Maine
                    (MD (DE PA WV DC VA))          ; MD = Maryland
                    (MA (RI CT NY VT NH))          ; MA = Mass
                    (MI (OH IN WI))                ; MI = Michigan
                    (MN (WI IA SD ND))             ; MN = Minnesota
                    (MS (LA AR TN AL))             ; MS = Mississippi
                    (MO (KS NE IA IL KY TN AR OK)) ; MO = Missouri
                    (MT (ID WY SD ND))             ; MT = Montana
                    (NE (WY SD IA MO KS CO))       ; NE = Nebraska
                    (NV (CA OR ID UT AZ))          ; NV = Nevada
                    (NH (ME MA VT))                ; NH = New Hampshire
                    (NJ (NY PA DE))                ; NJ = New Jersey
                    (NM (AZ UT CO OK TX))          ; NM = New Mexico
                    (NY (PA NJ CT MA VT))          ; NY = New York
                    (NC (VA TN GA SC))             ; NC = North Carolina
                    (ND (MT SD MN))                ; ND = North Dakota
                    (OH (PA WV KY IN MI))          ; OH = Ohio
                    (OK (TX NM CO KS MO AR))       ; OK = Oklahoma
                    (OR (WA ID NV CA))             ; OR = Oregon
                    (PA (NY NJ DE MD WV OH))       ; PA = Pennsylvania
                    (RI (CT MA))                   ; RI = Rhode Island
                    (SC (GA NC))                   ; SC = South Carolina
                    (SD (WY MT ND MN IA NE))       ; SD = South Dakota
                    (TN (AR MO KY VA NC GA AL MS)) ; TN = Tennessee
                    (TX (NM OK AR LA))             ; TX = Texas
                    (UT (CO NM AZ NV ID WY))       ; UT = Utah
                    (VT (NY MA NH))                ; VT = Vermont
                    (VA (NC TN KY WV MD DC))       ; VA = Virginia
                    (WA (ID OR))                   ; WA = Washington
                    (WV (KY OH PA MD VA))          ; WV = West Virginia
                    (WI (MN IA  IL MI))            ; WI = Wisconsin
                    (WY (ID MT SD NE CO UT))))     ; WY = Wyoming
  
                    
;; vertexDegree: Vertex -> Integer
;; where
;;   Vertex: List (ie the vertex and its list of neighbors)
;;
;; example usage:
;; (vertexDegree (SC (GA NC)))
;; => 2
;;
;; contributed by Josh
(defun vertexDegree (v)
  (length (nth 1 v)))

;; vertexName: Vertex -> Symbol
;; contributed by Josh
(defun vertexName (v)
  (first v))

;; vertexNeighbors: Vertex -> List<VertexName>
;; contributed by Josh
(defun vertexNeighbors (v)
  (second v))

;; vertexRemoveFromNeighbors: Vertex, Symbol -> Vertex
;; given a vertex "v" and a vertex-name "vName"
;; return a new vertex with "vName" removed from the neighbors of "v"
;;
;; ex.
;; (vertexRemovefromNeighbors '(SC (GA NC)) 'GA)
;; => '(SC (NC))
;;
;; contributed by Josh
(defun vertexRemoveFromNeighbors (v vName)
  (list (vertexName v)
        (remove vName (vertexNeighbors v))))

;; vertexHasDegreeZeronOrOne?: Vertex-> Boolean                         
;; where
;;   Vertex:  List (ie the vertex and its list of neighbors)
;;   Boolean: t or nil
;;
;; example usage:
;; (vertexHasDegreeZeroOrOne? (SC (GA NC)))
;; => nil
;; (vertexHasDegreeZeroOrOne? (AK ()))
;; => t
;;
;; contributed by Josh
(defun vertexHasDegreeZeroOrOne? (v)
    (<= (vertexDegree v) 1))


;; Graph -> Boolean
;;
;; contributed by Josh
(defun graphNotEmpty? (g)
  (> (length g) 0))

;; Graph, Vertex -> Graph
;; return a new graph with the vertex removed
;; the neighbor references to the removed vertex will also be removed 
;;
;; contributed by Josh
(defun graphRemoveVertex (g v)
  (let ((g1 '())
        (vName (vertexName v)))
    ;; iterate graph to remove vertex neighbor references
    ;; (as well as) remove the vertex itself
    (loop for v in g do
         (if (eq (vertexName v) vName)
             nil ;; do nothing (don't add to new graph)
             (setf g1 (append g1 (list (vertexRemoveFromNeighbors v vName))))))
    ;; return the new graph
    g1))

;; Graph, List<VertexName> -> Graph
;; return a new graph with the vertices removed
;; the neighbor references to the removed vertices will also be removed 
;;
;; contributed by Josh
(defun graphRemoveVertices (g vNames)
  (let ((g1 (copy-list g)))
    (loop for vName in vNames do
         (setf g1 (graphRemoveVertex g1 (graphGetVertex g vName))))
    g1))

;; Graph -> Graph
;; return a new graph with all vertices removed where degree == 0 or 1
;; the neighbor references to removed vertices will also be removed 
;; 
;; contributed by Josh
(defun graphPrune (g)
  (let ((g1 (copy-list g))
        (verticesToRemove '()))
    ;; NB. determining which vertices to remove must be a separate step from
    ;; destructively removing them from a graph (during iteration).
    ;; Probably didn't explain that well.
    ;;
    ;; get vertices to remove
    (loop for v in g do
         (if (vertexHasDegreeZeroOrOne? v)
             (setf verticesToRemove (append verticesToRemove (list v)))))

    ;; remove vertices
    (loop for v in verticesToRemove do
         (setf g1 (graphRemoveVertex g1 v)))

    ;; check that the pruning didn't result in vertices with degree 0 or 1
    (let ((validPruning 't))
      (loop for v in g1 when (vertexHasDegreeZeroOrOne? v) do
           (progn (setf validPruning nil) (return)))

      ;; if pruning didn't result in further 0 or 1 degree vertices
      (if validPruning
          ;; return the pruned graph
          g1
          ;; else prune further
          (graphPrune g1)))))

;; Graph -> Vertex
;; contributed by Josh & Matt
;; BUG fix by Matt
(defun graphGetMaxDegreeVertex (g)
  (if (graphNotEmpty? g)
      ;; assume first vertex is the max degree vertex
      (let* ((maxVertex (first g))
             (maxVertexDegree (vertexDegree maxVertex)))
        ;; iterate graph (looking for a higher degree vertex)
        (loop for v in g do
             ;; if we found a higher degree vertex
             (if (> (vertexDegree v) maxVertexDegree)
                 ;; update what the current max vertex/degree is
                 (progn
                   (setf maxVertex v)
                   (setf maxVertexDegree (vertexDegree v)))))
        maxVertex)))

;; Graph -> CutSet
;; where
;;   CutSet:  List<Symbol> (Vertex Name)
;; contributed by Josh
(defun graphGetCutset (g)
  (let ((cutset '())
        (g1 (copy-list g)))
    (loop do
         ;; prune the graph of vertices with degree == 0 || 1
         (setf g1 (graphPrune g1))
         ;; make sure the graph isn't empty (from the pruning)
         (if (not (null g1))
             ;; get the max degree vertex of the graph
             (let ((maxVertex (graphGetMaxDegreeVertex g1)))
               ;; append it to the cutset
               (setf cutset (append cutset (list (vertexName maxVertex))))
               ;; remove it from the graph
               (setf g1 (graphRemoveVertex g1 maxVertex))))
         ;; while the graph isn't empty
         while (graphNotEmpty? g1))
    ;; return the cutset
    cutset))

;; Graph, VertexName -> Vertex
;; contributed by Josh
(defun graphGetVertex (g vName)
  (loop
     for v in g
     when (eq vName (vertexName v))
     return v))

;; Graph -> VertexNames
;; contributed by Josh
(defun graphGetNames (g)
  (loop for v in g collect (vertexName v)))

;; Graph, List<VertexName> -> List<Vertex>
;; contributed by Josh
(defun graphConvertNamesToVertices (g vNames)
  (loop for vName in vNames collect (graphGetVertex g vName)))

;; Graph, Graph -> Graph
;; return the graph which is the difference of graphA - graphB
;; contributed by Josh
(defun graphDifference (graphA graphB)
  (let* ((aNames (graphGetNames graphA))
         (bNames (graphGetNames graphB))
         (cNames (set-difference aNames bNames))
         (graphC (graphConvertNamesToVertices graphA cNames)))
    graphC))

;; Graph, VertexName -> List<VertexName>
;; return a list of the vertices in the graph using a depth first search
;; NB. this only works on connected graphs, 
;;
;; contributed by Josh
(defun graphConnectedDFS (g startVName)
  (let ((visited '()) ;; the visited vertices
        (stack '()))  ;; the stack

    ;; push the first element on the stack
    (push startVName stack)
    
    ;; while the stack isn't empty
    (loop
       while (not (null stack))
       do
           ;; pop the stack
         (let ((vName (pop stack)))
           ;; visit the vertex (if it's not already visited)
           (if (not (member vName visited))
               (push vName visited))
           ;; push the vertex's (unvisited) neighbors
           (loop for vNeighborName in (vertexNeighbors (graphGetVertex g vName)) do
                (if (not (member vNeighborName visited))
                    (push vNeighborName stack)))))
    ;; return the visited vertices
    ;; (reverse it so the starting root vertex is at the first position)
    (reverse visited)))

;; Graph, VertexName ->  List<VertexName>
;; return a list of the vertices in the graph using a depth first search
;; NB. this works on *both* connected and disconnected graphs 
;;
;; contributed by Josh
(defun graphDFS (g vStartName)
  (let ((visited (graphConnectedDFS g vStartName))) ;; the visited vertices

    (loop
       ;; while we haven't visited every vertex in the graph
       while (not (eq (length visited) (length g)))
       ;; run DFS on the next unvisited vertex  
       do
         (let ((vNextStartName  (first (set-difference (graphGetNames g) visited))))
           (setf visited (append visited (graphConnectedDFS g vNextStartName)))))
    visited))


;; Graph -> ColorGraph
;; where
;;   ColorGraph:  List<ColorVertex>
;;   ColorVertex: List<VertexName, Neighbors, AvailableColors>
;; contributed by Josh
(defun graphToColorGraph (g)
    (loop for v in g collect 
          (list (vertexName v)
                (vertexNeighbors v)
                '()))) ;; initially unset color 

;; List<Whatever> -> Whatever
;; contributed by Josh
(defun nthRandom (lst)
    (if (not (null lst))
        (let* ((randomIndex   (random (length lst)))
               (randomElement (nth randomIndex lst)))
          randomElement)))

;; Color
;; contributed by Josh
(defun randomColor () (nthRandom *colors*))

;; Number -> List<Color>
;; contributed by Josh
(defun randomNoRepeatColors (n)
  (if (>= n 1)
      (let ((lst (list (randomColor))))
        (loop 
           while (< (length lst) n)
           do (let ((rc (randomColor)))
                (if (not (eq (first lst) rc))
                    (push rc lst))))
        lst)))
 
;; ColorVertex -> List<Color>
;; contributed by Josh
(defun colorVertexGetColors (cv)
  (third cv))


;; ColorVertex, Color -> ColorVertex
;; given a color vertex, return a new color vertex
;; with "color" absent from its possible colors
;; contributed by Josh
(defun colorVertexRemoveColor (cv color)
  (list (vertexName cv)
        (vertexNeighbors cv)
        (remove color (colorVertexGetColors cv))))

;; ColorVertex, Color -> ColorVertex
;; given a color vertex, return a new color vertex with only one color set
;; contributed by Josh
(defun colorVertexSetColor (cv color)
  (list (vertexName cv)
        (vertexNeighbors cv)
        (list color)))

;; ColorGraph, Name -> ColorVertex
;; get the associated colorGraphVertex at Name (else nil)
;; contributed by Josh
(defun colorGraphGetColorVertex (cg vName)
  (loop
     for v in cg
     when (eq vName (vertexName v))
     return v))

;; ColorGraph, List<VertexName> -> List<ColorVertex>
;; contributed by Josh
(defun colorGraphConvertNamesToVertices (cg vNames)
  (loop for vName in vNames collect (colorGraphGetColorVertex cg vName)))

;; ColorGraph -> Boolean
;; NB. this function assumes each vertex in the color graph is of length 1
;; (that is to say it's only 1 color)
;; contributed by Josh
(defun colorGraphIsValid? (cg)
  ;;
  ;; for each vertex in the color graph
  ;; check that its neighbors don't have its color
  ;;
  (let ((validity 't)) ;; assume true
    ;; for each vertex in the graph
    (loop for v in cg do
         ;; get the vertex's color and neighbor names
         (let ((vNeighborNames (vertexNeighbors v))
               (vColor (first (colorVertexGetColors v))))
           ;; check color isn't nil
           (if (null vColor)
               ;; break loop (returning false) if it is nil
               (progn
                 (setf validity nil)
                 (return)))
           ;; for each neighbor name
           (loop for vNeighborName in vNeighborNames do
                ;; get the neighbor vertex's colors
                (let* ((vNeighbor (colorGraphGetColorVertex cg vNeighborName))
                       (vNeighborColors (colorVertexGetColors vNeighbor)))
                  ;; if our vertex's color is in the neighbor vertex's colors
                  (if (member vColor vNeighborColors)
                      ;; break loop (returning false)
                      (progn
                        (setf validity nil)
                        (return)))))))
    validity))
                      

;; ColorGraph, SubSet -> Boolean
;; where
;;   SubSet: List<VertexName>
;; NB. this function assumes each subset vertex in the color graph
;; is of length 1 (that is to say it's only 1 color)
;; vertices in the color graph which *aren't* subset vertices are ignored
;; contributed by Josh
(defun colorGraphIsValidSubSet? (cg subsetNames)
  (let ((validity 't)) ;; assume true
    ;; for each subset vertex in the graph
    (loop for vName in subsetNames do
         ;; get the vertex's color and neighbor names
         (let* ((v (colorGraphGetColorVertex cg vName))
                (vNeighborNames (intersection subsetNames (vertexNeighbors v)))
                (vColor (first (colorVertexGetColors v))))
           ;; for each neighbor name (which is in the subsetNames)
           (loop for vNeighborName in vNeighborNames do
                ;; get the neighbor vertex's colors
                (let* ((vNeighbor (colorGraphGetColorVertex cg vNeighborName))
                       (vNeighborColors (colorVertexGetColors vNeighbor)))
                  ;; if our vertex's color is in the neighbor vertex's colors
                  ;; or vertex's colors are '()
                  (if (or (member vColor vNeighborColors)
                          (null vNeighborColors))
                      ;; break loop (returning false)
                      (progn
                        (setf validity nil)
                        (return)))))))
    validity))

;; ColorGraph, SubSet -> ColorGraph
;; where
;;   SubSet: List<VertexName>
;; WARNING: this might produce illegal color graphs
;; it must be checked elsewhere
;; contributed by Josh
(defun colorGraphRandomlyColorSubSet (colorGraph subsetNames)
  (let ((colorGraphWithValidSubSet nil))
    (loop for i below 1000 do
       ;; copy the color graph 
       (let ((cg (copy-list colorGraph)))
         ;; iterate the subset vertex names
         ;; and randomly color them
         (loop for subsetName in subsetNames do
              ;; get a random color and assign it
              (setf cg (colorGraphAssignAvailableColor cg subsetName)))
         ;; check that the colored subset is valid
         (if (not (colorGraphIsValidSubSet? cg subsetNames))
             ;; if it's not valid, retry
             nil
             ;; else it's valid
             (progn
               ;; announce it's valid
               ;; save the valid cutset color graph
               (setf colorGraphWithValidSubSet cg)
               ;; break out of our tries loop
               (return)))))
    
    ;; return the valid cutset color graph
    ;; or possibly nil if it didn't succeed
    colorGraphWithValidSubSet))

;; ColorGraph -> ()
;; just pretty print it
(defun printColorGraph (cg)
  (format t "~%~A~%" (loop for cv in cg collect
                          (format nil
                                  "  ~A, ~A, ~A~%"
                                  (vertexName cv)
                                  (vertexNeighbors cv)
                                  (colorVertexGetColors cv)))))


;; ColorGraph, VertexName, Color -> ColorGraph
;; set vertex color without error checking
;;
;; contributed by Josh
(defun colorGraphSetVertexColor (colorGraph vName color)
  (let ((cg '()))
    ;; iterate the vertices
    (loop for cv in colorGraph do
         ;; if we found our vertex
         (if (eq (vertexName cv) vName)
             ;; append the updated vertex
             (setf cg (append cg (list (colorVertexSetColor cv color))))
             ;; else append the rest of the vertices
             (setf cg (append cg (list cv)))))
    cg))
             

;; ColorGraph, VertexName -> ColorGraph
;; picks an available color for the specified vertex
;; NB. this is for usage within code below and shouldn't be called otherwise
;;
;; contributed by Josh
(defun colorGraphAssignAvailableColor (cg vName)
  (let* ((v (colorGraphGetColorVertex cg vName))
         (vNeighborNames (vertexNeighbors v))
         (unavailableColors '())
         (availableColors *colors*))
    ;; get which colors are unavailable
    (loop for vNeighborName in vNeighborNames do
         (setf unavailableColors
               (union unavailableColors
                      (colorVertexGetColors (colorGraphGetColorVertex cg vNeighborName)))))
    ;; get which colors are available
    (setf availableColors (set-difference availableColors unavailableColors))
    ;; set the color 
    (colorGraphSetVertexColor cg vName (nthRandom availableColors))))
    

;; Graph -> ColorGraph
;;
;; Color a graph
;; (the main program's algorithm)
;;
;;
;; get cutset
;; (randomly) color cutset
;; remove cutset colors from cutset neighbors's available colors
;; get a dfs tour of the remainder graph (the graph with cutset removed)
;; for each vertex in order of the dfs tour
;;   color the remainder graph with its remaining available colors
;; return the colored graph
;;
;; contributed by Josh
(defun doMapColoring (graph)
  ;; get the cutset for this graph and
  ;; get a dfs tour of the remainder graph (the graph with cutset removed)
  (let* ((cutset (graphGetCutset graph))
         (remainderGraph (graphRemoveVertices graph cutset))
         (remainderTour (graphDFS remainderGraph (vertexName (first remainderGraph)))))
    (loop
       do
         ;; convert graph to a color graph (and randomly color the cutset)
         (let ((cg (colorGraphRandomlyColorSubSet (graphToColorGraph graph) cutset)))
           ;; for each remainder vertex (in order) of the dfs tour
           (loop for r in remainderTour do
                ;; color the rest of the graph with each vertex's remaining available colors
                (setf cg (colorGraphAssignAvailableColor cg r)))
           ;; return a (valid) color graph
           (if (colorGraphIsValid? cg)
               (return cg))))))

;; test doMapColoring
(format t "~%----------------------------------------~%~%")
(format t "Coloring *map-1*~%")
(printColorGraph (doMapColoring *map-1*))
(format t "~%----------------------------------------~%~%")
(format t "Coloring *50-states*~%")
(printColorGraph (doMapColoring *50-states*))
(format t "~%----------------------------------------~%~%")
