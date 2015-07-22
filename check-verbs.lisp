(defparameter *verbs*
  '("deletar" "extirpar" "erradicar" "subtrair" "descarregar" "remover"
"extrair" "arrancar" "recolher" "eliminar" "cortar" "liberar"
"retirar" "tirar" "isolar" "destituir" "excluir" "expulsar" "separar"
"abolir" "afastar" "apagar" "demitir" "derrubar" "despedir" "destacar"
"exonerar" "exterminar" "extinguir" "sacar" "suprimir" "reenviar"
"realocar" "despachar" "transportar" "remeter" "desviar" "emitir"
"transmitir" "transferir" "enviar" "mandar" "deslocar" "trazer"
"encaminhar" "expedir" "remeter" "fretar" "arranjar" "colher"
"reservar" "adquirir" "pegar" "obter" "buscar" "comprar" "ganhar"
"conseguir" "alugar" "contratar" "arrendar" "conquistar" "angariar"
"apanhar" "arrematar" "captura" "alcanar" "atingir" "chamar"
"conquistar" "encomendar" "escolher" "juntar" "ligar" "marcar"
"reunir" "tirar" "angariar" "apanhar" "arrancar" "arrematar" "auferir"
"caar" "capturar" "catar" "embolsar" "extrair" "guardar" "lucrar"
"pinar" "puxar" "segurar" "selecionar" "telefonar" "tapear"
"esbofetear" "chicotear" "esmurrar" "surrar" "socar" "martelar"
"aoitar" "golpear" "esmagar" "espancar" "bater" "golpear" "alvejar"
"amassar" "coar" "comprimir" "cutucar" "esbarrar" "correlacionar"
"unificar" "alternar" "contrastar" "coincidir" "combinar" "juntar"
"unir" "comparar" "reunir" "bater" "integrar" "ligar" "prender"
"aderir" "aliar" "atar" "atrelar" "combinar" "coordenar" "grudar"
"harmonizar" "interligar" "mesclar" "misturar" "relacionar" "somar"
"ornar" "enlistar" "diagnosticar" "pintar" "retratar" "encarar"
"classificar" "descrever" "identificar" "definir" "representar"
"tratar" "aceitar" "adotar" "caracterizar" "reconhecer" "conceber"
"conhecer" "considerar" "dar" "entender" "ter" "tomar" "ver"
"imaginar" "interpretar" "julgar" "olhar" "recordar" "rememorar"
"reproduzir" "supor" "visualizar" "perceber" "tomar" "conhecer"
"enxergar" "lembrar" "taxar" "achar" "pertubar" "frustar" "atemorizar"
"chatear" "alarmar" "escandalizar" "aterrorizar" "amedrontar"
"deliciar" "apavorar" "alegrar" "decepcionar" "aborrecer" "encantar"
"ofender" "espantar" "chocar" "contrariar" "abalar" "agradar"
"assustar" "confundir" "estimular" "ferir" "ameaar" "abater" "acalmar"
"afetar" "animar" "atrair" "atrapalhar" "cegar" "comover" "consolar"
"desanimar" "desgraar" "deslumbrar" "divertir" "emocionar" "empolgar"
"excitar" "fascinar" "iludir" "impressionar" "incomodar" "indignar"
"induzir" "inquietar" "inspirar" "intimidar" "intrigar" "irritar"
"prejudicar" "preocupar" "revoltar" "afligir" "aliviar" "atormentar"
"cativar" "confortar" "constranger" "contentar" "deprimir" "desagradar"
"desapontar" "descontentar" "desencorajar" "desestimular" "desonrar"
"distrair" "embaraar" "encorajar" "enfurecer" "entreter" "entusiasmar"
"envergonhar" "horrorizar" "humilhar" "incendiar" "inflamar"
"penalizar" "perturbar" "sensibilizar" "tranquilizar" "satisfazer"
"agitar" "arrasar" "cansar" "enfraquecer" "estremecer" "maravilhar"
"serenar" "sossegar" "torturar" "acabrunhar" "acalentar" "alucinar"
"aprazer" "arrepiar" "comprazer" "debilitar" "deleitar" "desconfortar"
"desesperar" "desgostar" "enfadar" "entristecer" "esmorecer" "exaltar"
"exaurir" "exultar" "fatigar" "impacientar" "importunar" "magoar"
"oprimir" "tolher" "vexar" "zangar" "regatear" "pechinchar" "flertar"
"simpatizar" "colidir" "cooperar" "interagir" "namorar" "colaborar"
"casar" "concordar" "debater" "discutir" "brigar" "conviver" "unir"
"discordar" "acordar" "ajustar" "combinar" "compactuar" "comunicar"
"conciliar" "conspirar" "disputar" "negociar" "pactuar" "paquerar"
"relacionar" "reunir" "recontar" "reportar" "retrucar" "proclamar"
"exprimir" "replicar" "formular" "noticiar" "narrar" "mencionar"
"confiar" "expor" "relatar" "alegar" "declarar" "anunciar" "responder"
"falar" "apresentar" "afirmar" "dizer" "comunicar" "contar" "ordenar"
"propor" "citar" "divulgar" "informar" "lembrar" "repetir" "sugerir"
"confidenciar" "notificar" "reiterar" "exclamar" "insinuar"
"verbalizar" "revelar" "choramingar" "grunhir" "cochichar" "rosnar"
"sussurrar" "segredar" "berrar" "resmungar" "balbuciar" "bradar"
"murmurar" "queixar" "gritar" "assobiar" "balir" "bufar" "cacarejar"
"cantar" "ciciar" "chiar" "chilrear" "coaxar" "delirar" "gaguejar"
"ganir" "gemer" "gorgolejar" "gorjear" "grasnir" "grunhir" "guinchar"
"ladrar" "latir" "miar" "mugir" "murmurar" "piar" "ralhar" "ranger"
"relinchar" "roncar" "ronronar" "rugir" "segredar" "sibilar" "silvar"
"soprar" "suspirar" "uivar" "vaiar" "vociferar" "zumbir" "zurrar"
"gargalhar" "bocejar" "arfar" "roncar" "soluar" "suspirar" "sorrir"
"chorar" "rir" "assobiar" "cheirar" "choramingar" "farejar" "gargalhar"
"gemer" "ofegar" "piscar" "rosnar" "soluar" "sorrir" "suspirar"
"tremeluzir" "chamejar" "refulgir" "flamejar" "reluzir" "resplandecer"
"luzir" "raiar" "cintilar" "piscar" "brilhar" "refletir" "alumiar"
"faiscar" "lucilar" "amolecer" "suavizar" "encurtar" "afrouxar"
"alargar" "estreitar" "derreter" "dissipar" "congelar" "dissolver"
"afundar" "enfraquecer" "aprofundar" "contrair" "fundir" "intensificar"
"ampliar" "abaixar" "abrandar" "acalmar" "acordar" "afiar" "afinar"
"agigantar" "alagar" "alegrar" "alisar" "alongar" "alvejar" "amaciar"
"amortecer" "amplificar" "apagar" "aperfeioar" "aplacar" "apressar"
"aprimorar" "arrebentar" "arrefecer" "arrombar" "atenuar" "avolumar"
"avultar" "azedar" "caiar" "cegar" "cicatrizar" "clarear" "comprimir"
"condensar" "corar" "cristalizar" "curar" "debilitar" "degenerar"
"deprimir" "desacelerar" "desandar" "desatar" "descentralizar"
"desdobrar" "desenrolar" "desequilibrar" "desestabilizar" "desfiar"
"destravar" "dilatar" "diluir" "dourar" "encurtar" "endurecer"
"entreabrir" "envelhecer" "enxugar" "equilibrar" "escurecer" "esfriar"
"estabilizar" "estalar" "esticar" "estourar" "estrangular" "expandir"
"explodir" "fortalecer" "furar" "gelar" "inundar" "manchar" "matizar"
"melar" "molhar" "nivelar" "reacender" "reanimar" "reavivar"
"recrudescer" "refrescar" "ressecar" "secar" "solidificar" "sujar"
"umedecer" "vergar" "abalar" "abrir" "acelerar" "acender" "agravar"
"apontar" "aquecer" "despertar" "detonar" "encher" "encolher"
"engrossar" "esquentar" "fechar" "lotar" "serpear" "tremular"
"rodopiar" "boiar" "flutuar" "vibrar" "oscilar" "abanar" "esvoaar"
"agitar" "balanar" "brandir" "danar" "girar" "pairar" "palpitar"
"pulsar" "sacudir" "tremer" "inclinar" "ondular" "pender" "rebolar"
"trotar" "galopar" "planar" "perambular" "escorregar" "deslizar"
"marchar" "nadar" "passear" "voar" "caminhar" "andar" "correr"
"avanar" "desfilar" "flutuar" "pular" "saltar" "subir" "vagar"
"engatinhar" "escalar" "galgar" "mergulhar" "boiar" "esvoaar"
"mancar" "cambalear" "cavalgar" "escalar" "esgueirar" "rastejar"
"saltitar" "sonambular" "trotar" "ziguezaguear" "gatinhar" "cravar"
"posicionar" "mergulhar" "situar" "inserir" "depositar" "introduzir"
"meter" "guardar" "instalar" "montar" "pr" "colocar" "botar"
"implantar" "incluir" "enterrar" "fincar" "fixar" "plantar" "dispor"
"encaixar" "estacionar" "intercalar" "semear"))

(ql:quickload :drakma)
(ql:quickload :yason)
(ql:quickload :alexandria)

;; (setf drakma:*header-stream* *standard-output*)

(defvar *server* "localhost:8040")

(defun search-activities (term)
    (let* ((stream (drakma:http-request
                   (format nil "http://~a/wn/search-activities?sf=&so=&term=~a&start=0&fq_action=add-word-pt" *server* (drakma:url-encode term :utf-8))
		   :external-format-out :utf-8
                   :accept "application/json"
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :alist
			    :object-key-fn #'alexandria:make-keyword)))
      (close stream)
      obj)))

(defun search-wn (term)
    (let* ((stream (drakma:http-request
                   (format nil "http://~a/wn/search?term=word_pt:~a" *server* (drakma:url-encode term :utf-8))
		   :external-format-out :utf-8
                   :accept "application/json"
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :alist
			    :object-key-fn #'alexandria:make-keyword)))
      (close stream)
      obj)))

(dolist (v *verbs*)
  (let ((wn (cdr (assoc :NUMFOUND (search-wn v))))
        (ac (cdr (assoc :NUMFOUND (search-activities v)))))
    (when (and (= 0 wn) (= 0 ac))
        (format t "~a not in OWN-PT~%" v))
    (when (> wn 0)
      (format t "~a in OWN-PT~%" v))
    (when (> ac 0)
      (format t "~a in SUGGESTIONS.~%" v))))
