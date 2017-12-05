

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
(defun vertexDegree (v)
  (length (nth 1 v)))

;; vertexName: Vertex -> Symbol
(defun vertexName (v)
  (first v))

;; vertexNeighbors: Vertex -> List<VertexName>
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
(defun vertexHasDegreeZeroOrOne? (v)
    (<= (vertexDegree v) 1))


;; Graph -> Boolean
;;
(defun graphNotEmpty? (g)
  (> (length g) 0))

;; Graph, Vertex -> Graph
;; return a new graph with the vertex removed
;; the neighbor references to the removed vertex will also be removed 
;;
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

;; Graph -> Graph
;; return a new graph with all vertices removed where degree == 0 or 1
;; the neighbor references to removed vertices will also be removed 
;; 
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
    g1))

;; Graph -> Vertex
(defun graphGetMinimumVertex (g)
  (if (graphNotEmpty? g)
      ;; iterate graph looking for the minimum vertex
      (let* ((minimumVertex (first g))
             (minimumVertexDegree (vertexDegree minimumVertex)))
        ;; because w(v) == 1 for our particular problem, we don't need to calculate
        ;; w(v)/d(v) and can just instead compare d(v)
        ;;
        ;; iterate graph
        (loop for v in g do
             (if (> (vertexDegree v) minimumVertexDegree)
                 ;; update what are current minimum vertex/degree is
                 (setf minimumVertex v)
                 (setf minimumVertexDegree (vertexDegree v))))
        minimumVertex)))

;; Graph -> CutSet
;; where
;;   CutSet:  List<Symbol> (Vertex Name)
(defun graphGetCutset (g)
  (let ((cutset '())
        (g1 (copy-list g)))
    (loop do
         (format t "g1: ~A~%" g1) ;; debug
         (format t "cutset: ~A~%" cutset) ;; debug
         ;; prune the graph of vertices with degree == 0 || 1
         (setf g1 (graphPrune g1))
         (format t "pruned g1: ~A~%" g1) ;; debug
         ;; make sure the graph isn't empty (from the pruning)
         (if (not (null g1))
             ;; get the minimum vertex of the graph
             (let ((minimumVertex (graphGetMinimumVertex g1)))
               (format t "minimum vertex: ~A~%" minimumVertex) ;; debug
               ;; append the minimum vertex to the cutset
               (setf cutset (append cutset (list (vertexName minimumVertex))))
               ;; remove the minimum vertex from the graph
               (setf g1 (graphRemoveVertex g1 minimumVertex))
               (format t "g1:~A~%" g1) ;; debug

               ))
         ;; while the graph isn't empty
         while (graphNotEmpty? g1))
    ;; return the cutset
    cutset))

;; Graph, VertexName -> Vertex
(defun graphGetVertex (g vName)
  (loop
     for v in g
     when (eq vName (vertexName v))
     return v))

;; Graph, List<VertexName> -> List<Vertex>
(defun graphConvertNamesToVertices (g vNames)
  (loop for vName in vNames collect (graphGetVertex g vName)))

;; Graph -> VertexNames
(defun graphGetNames (g)
  (loop for v in g collect (vertexName v)))

;; Graph, VertexName -> Graph
(defun graphMakeFromNames (g vNames)
  (loop for vName in vNames collect (graphGetVertex g vName)))

;; Graph, Graph -> Graph
;; return the graph which is the difference of graphA - graphB
(defun graphDifference (graphA graphB)
  (let* ((aNames (graphGetNames graphA))
         (bNames (graphGetNames graphB))
         (cNames (set-difference aNames bNames))
         (graphC (graphMakeFromNames graphA cNames)))
    graphC))

;; Graph -> ColorGraph
;; where
;;   ColorGraph:  List<ColorVertex>
;;   ColorVertex: List<VertexName, Neighbors, AvailableColors>
(defun graphToColorGraph (g)
    (loop for v in g collect 
          (list (vertexName v)
                (vertexNeighbors v)
                (copy-list *colors*))))

;; List<Whatever> -> Whatever
(defun nthRandom (lst)
  (let* ((randomIndex   (random (length lst)))
         (randomElement (nth randomIndex lst)))
    randomElement))

;; Color
(defun randomColor () (nthRandom *colors*))

;; ColorVertex -> List<Color>
(defun colorVertexGetColors (cv)
  (third cv))


;; ColorVertex, Color -> ColorVertex
;; given a color vertex, return a new color vertex with "color"
;; absent from its possible colors
(defun colorVertexRemoveColor (cv color)
  (list (vertexName cv)
        (vertexNeighbors cv)
        (remove color (colorVertexGetColors cv))))

;; ColorVertex, Color -> ColorVertex
;; given a color vertex, return a new color vertex with only one color set
(defun colorVertexSetColor (cv color)
  (list (vertexName cv)
        (vertexNeighbors cv)
        (list color)))

;; ColorGraph, Name -> ColorVertex
;; get the associated colorGraphVertex at Name (else nil)
(defun colorGraphGetColorVertex (cg vName)
  (loop
     for v in cg
     when (eq vName (vertexName v))
     return v))

;; ColorGraph, List<VertexName> -> List<ColorVertex>
(defun colorGraphConvertNamesToVertices (cg vNames)
  (loop for vName in vNames collect (colorGraphGetColorVertex cg vName)))

;; ColorGraph -> ColorGraph
;; make the colorgraph have one color option per vertex
;;
(defun colorGraphRender (cg)
    (loop for v in cg collect
          (list (vertexName v)
                (vertexNeighbors v)
                (list (first (third v)))))) ;; '(R G B) -> '(R)

;; ColorGraph -> Boolean
;; NB. this function assumes each vertex in the color graph is of length 1
;; (that is to say it's only 1 color)
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
               ;; break loop (returning false)
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

;; ColorGraph, VertexName, Color -> ColorGraph
;; given a color graph
;; assign a color to a specific vertex "vName"
;; and return the newly updated color graph
;
;; WARNING: THIS DOES NO CONFLICT CHECKING
;; that must be handled wherever this is called
;;
(defun colorGraphAssignColor (colorGraph vName color)
  ;;
  ;; assign the color to this color vertex by:
  ;;   removing the color from the other neighbor vertices in the color graph
  ;;   setting this color at the specific vertex in question
  ;;
  (let* ((cg '())
         (colorVertex (colorGraphGetColorVertex colorGraph vName))
         (colorVertexNeighbors (vertexNeighbors colorVertex)))

    ;; iterate graph
    (loop for cv in colorGraph do

         (let ((currentVName (vertexName cv))
               (vertexToAppend nil))

           (cond
             ;; if we're at our vertex
             ((eq currentVName vName)
              ;; set its color
              (setf vertexToAppend (colorVertexSetColor cv color)))

             ;; if we're at the vertex's neighbor
             ((member currentVName colorVertexNeighbors)
              ;; remove the color from the neighbor's possible colors
              (setf vertexToAppend (colorVertexRemoveColor cv color)))

             ;; else if we're not on the vertex or its neighbors
             ;; do nothing
             (t (setf vertexToAppend cv)))

           ;; append the new vertex
           (setf cg (append cg (list vertexToAppend)))))

    ;; return the updated color graph
    cg))

  
;; ColorGraph, SubSet -> ColorGraph
;; where
;;   SubSet: List<VertexName>
;; WARNING: this might produce illegal color graphs
;; it must be checked elsewhere
(defun colorGraphRandomlyColorSubSet (colorGraph subsetNames)
  (let ((colorGraphWithValidSubSet nil))
    (loop for i below 1000 do
       ;; copy the color graph 
       (let ((cg (copy-list colorGraph)))
         ;; iterate the subset vertex names
         ;; and randomly color them
         (loop for subsetName in subsetNames do
              ;; get a random color and assign it
              (setf cg
                    (colorGraphAssignColor cg subsetName (randomColor))))
         ;; check that the colored subset is valid
         (if (not (colorGraphIsValidSubSet? cg subsetNames))
             ;; if it's not valid, retry
             (format t ">> It's not valid (subset) coloring... retrying~%") ;; DEBUG
             ;; else it's valid
             (progn
               ;; announce it's valid
               (format t ">> Got a valid (subset) coloring!~%") ;; DEBUG
               ;; save the valid cutset color graph
               (setf colorGraphWithValidSubSet cg)
               ;; break out of our tries loop
               (return)))))
    
    ;; return the valid cutset color graph
    ;; or possibly nil if it didn't succeed
    colorGraphWithValidSubSet))

;; ColorGraph -> ColorGraph
;; higher degress... lower degrees
(defun colorGraphSortByDegreeDescending (colorGraph)
  (sort (copy-list colorGraph) #'> :key #'vertexDegree))
    
;; ColorGraph, RemainderVertices -> ColorGraph
;; where
;;  Remaindervertices: List<VertexName>
;; assumes the complement of the remainder is colored already
;;
;; WARNING: may fail (produce a graph which isn't legal)
;; and returns nil on failure

;; (defun colorGraphColorRemainderVertices (colorGraph remainderVertexNames)
;;   (let ((colorGraphValid nil)
;;         (cg (copy-list colorGraph)))
;;            ;; sort by degrees descending
;;            (setf cg (colorGraphSortByDegreeDescending cg))
;;            ;; iterate sorted color graph
;;            (loop for v in cg do
;;                 ;; skip vertex if not a member of remainder vertices
;;                 (if (member (vertexName v) remainderVertexNames)
;;                     (
;;                     ;; get a possible color
;;                     ;; if it can't be legally assigned, fail and return nil
                    
;;                     )
                
           
;;            )
;;     colorGraphValid))

;;TODO
;; Graph -> ColorGraph
(defun doMapColoring (graph)
  ;; convert the graph into a color graph (to hold color state)
  (let ((cg (graphToColorGraph graph))
        (cutset (graphGetCutset graph)))
    ;; create random colors for the cutset
    (setf cg (colorGraphRandomlyColorSubSet cg cutset))
    ;; proceed if the returned cutset colored graph is valid
    (if (not (null cg))
        (let ((remainderVertexNames (set-difference
                                     (graphGetNames graph)
                                     cutset)))
          ;; color the remainder vertices
          (setf cg (colorGraphColorRemainderVertices cg remainderVertexNames))))
    ;; return (valid) color graph (or nil)
    cg))

;; test graphGetCutset
(format t "~%~%~%;--------------------------------~%")
(format t "~A:~%=> ~S~%" "*map-1*"     (graphGetCutset *map-1*))
;; (format t "~A:~%=> ~S~%" "*50-states*" (graphGetCutset *50-states*))
(format t "--------------------------------;~%")

;; ;; test colorGraphRandomlyColorSubSet 
;; (format t "~%~%~%;--------------------------------~%")
;; (format t
;;         "~A~%=> ~S~%"
;;         "randomly colored graph of *map-1*"
;;         (colorGraphRandomlyColorSubSet (graphToColorGraph *map-1*) ;; color graph
;;                                        (graphGetCutset *map-1*)))  ;; vertex names
;; (format t "--------------------------------;~%")
;; 
;; ;; test graphDifference
;; (format t "~%~%~%;--------------------------------~%")
;; (format t
;;         "~A~%=> ~S~%"
;;         "graphDifference: *map-1* - the cutset of *map-1*"
;;         (graphDifference *map-1* (graphMakeFromNames *map-1* (graphGetCutset *map-1*))))
;; (format t "--------------------------------;~%")
;; 
;; ;; test doMapColoring
;; (format t "~%~%~%;--------------------------------~%")
;; (format t
;;         "~A:~%=> ~S~%"
;;         "*coloring map-1*"
;;         (doMapColoring *map-1*))
;; (format t
;;         "~A:~%=> ~S~%"
;;         "*coloring *50-states*"
;;         (doMapColoring *50-states*))
;; (format t "--------------------------------;~%")
