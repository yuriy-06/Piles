"
класс, который  должен генерировать текстовый файл Лиры:
doc0 -- заголовок документа
doc1 -- документ конечных элементов
doc3 -- документ жесткостей
doc4 -- документ узлов
doc5 -- фиктивная связь
doc6 --//-- (типы нагрузок)
doc7 --//--(величины нагрузок)
doc19 -- документ к-в пастели

"
Class {
	#name : #LiraFile,
	#superclass : #Object,
	#instVars : [
		'doc0',
		'doc1',
		'doc3',
		'doc4',
		'doc5',
		'doc6',
		'doc7',
		'doc19',
		'nodesNumbers',
		'lastUsedNode',
		'liraDoc',
		'keBase',
		'nodesBase'
	],
	#category : #Piles
}

{ #category : #'as yet unclassified' }
LiraFile class >> czStringGenNum: num bp: bp cz: cz [
	| val |
	val := ' {1} {2} 0' format: {bp . cz}.
	val := num asString , val , val , '/' , String crlf .
	^ val
]

{ #category : #'as yet unclassified' }
LiraFile class >> doc0 [
"заголовок лировского файла"
^
'( 0/ 1; 123/ 2; 5/28; 0 1 0  1 0 0  0 0 1; /33;M 1 CM 100 T 1 C 1 /39;
 1: load 1 ; /)'
]

{ #category : #'as yet unclassified' }
LiraFile class >> doc19genM: m [
	| col igeList num doc19str cz bp z2 l alfa |
	doc19str := '(19/'.
	num := 1.
	m
		do: [ :pile | 
			l := pile l.
			bp := pile bp round: 3.
			col := pile col.
			igeList := col igelistOut.
			alfa := pile alfaK.
			igeList
				do: [ :ige | 
					z2 := ige z2.
					z2 <= l
						ifTrue: [ cz := ige cz * alfa round: 3.
							doc19str := doc19str , (LiraFile czStringGenNum: num bp: bp cz: cz).
							num := num + 1 ] ].
			num := num + 1	"пропуск КЕ 51" ].
	doc19str := doc19str , ')' , String crlf.
	^ doc19str
]

{ #category : #'as yet unclassified' }
LiraFile class >> doc1MpilesM: m stiffStem: stemStiffBase czStiff: czStiffBase [
|keString col igelist nodesNumber cz baseStiff stemStiff n nn kebase keGen stringOut keGenBlock lPile
z2|
kebase := OrderedCollection new.
baseStiff := stemStiffBase clone.
n := (baseStiff stiffOut). nn := (czStiffBase stiffOut ).
n addAll: nn. "добавили к базе вторую базу"
baseStiff := n.
keString := ''.
nodesNumber := 1.
"генерирует элементы -- документ 1 Лиры"
keGenBlock := [ kebase add: keGen .
			keString := keString , keGen .
			nodesNumber := nodesNumber + 1 . ].
m do: 
	[:eachPile | 
		lPile := eachPile l. 
		col := eachPile col. igelist := col igelistOut . stemStiff := eachPile stemStiffness .
		igelist do: 
			[ :ige |  z2 := ige z2.
			(z2 <= lPile ) ifTrue: [ 
				keGen := LiraFile  ke10StringGenNum: nodesNumber  jest: ((stemStiffBase stiffOut ) indexOf: stemStiff ).
				keGenBlock value ]].
		cz := (eachPile CallOut) asInteger . "округляем жесткость до целых, в базе она хранится тоже в целых"
		stemStiff := eachPile stemStiffness .
		keGen := LiraFile ke51StringGenNum: nodesNumber stiff: (baseStiff indexOf: cz).
		keGenBlock value.
		"сгенерили жесткость под концом сваи"
	].
Transcript show: ('lastUsedNodes: ', (nodesNumber asString) ).
stringOut := '
( 1/', keString , ')' .
^ { stringOut . kebase  }
]

{ #category : #'as yet unclassified' }
LiraFile class >> doc3Mpiles: m [
	| cz st1 st2 stemStiff czStiff iNumber |
	st1 := Stiffness new.	"жесткости сваи -- нумерованный список"
	st2 := Stiffness new.	"жесткости Cz -- нумерованный список "
	stemStiff := ''.	"жесткости сваи -- сгенерированная строка"
	czStiff := ''.	"жесткости Cz --  сгенерированная строка"
	"генерирует жесткости -- документ 3 Лиры"
	m
		do: [ :eachPile | 
			st1 add: eachPile stemStiffness.
			cz := eachPile CallOut asInteger.
			st2 add: cz ].
	"окей, получили базу, теперь надо прогнать ее, и на ее основе получить, тестовый док. 3,
вовращать придется массив из трех значений: 
коллекция жесткостей ствола, коллекция жесткостей Cz, строка док-та 3"
	iNumber := 1.
	st1 stiffOut
		do: [ :elem | 
			stemStiff := stemStiff
				, (LiraFile stemStringStiffGenNumm: iNumber stiff: elem).
			iNumber := iNumber + 1 ].
	st2 stiffOut
		do: [ :elem | 
			czStiff := czStiff , (LiraFile ke51StringGenStiffNum: iNumber stiff: elem).
			iNumber := iNumber + 1 ].
	^ {st1.
	st2.
	('( 3/' , stemStiff , czStiff , ')')}
]

{ #category : #'as yet unclassified' }
LiraFile class >> doc4Mpiles: m [
	| xy x y col igelist z1 z2 collectNodesString nodesNumbers nodesbase stringOut blockNodeGen lPile |
	nodesbase := OrderedCollection new.
	nodesNumbers := 1.
	collectNodesString := ''.
	"генерирует узлы -- документ 4 Лиры"
	blockNodeGen := [ :z | 
	| nodeGen |
	nodeGen := self nodesStringGenX: x y: y z: z negated.
	nodesbase add: nodeGen.
	collectNodesString := collectNodesString , nodeGen.
	nodesNumbers := nodesNumbers + 1 ].
	m
		do: [ :eachPile | 
			xy := eachPile xy.
			x := xy x.
			y := xy y.
			lPile := eachPile l.
			col := eachPile col.
			igelist := col igelistOut.
			igelist
				do: [ :ige | 
					z1 := ige z1.
					z2 := ige z2.
					z1 <= lPile
						ifTrue: [ blockNodeGen value: z1 ] ].
			z2 <= lPile
				ifTrue: [ blockNodeGen value: z2 ] ].
	Transcript
		show: ' число узлов: ' , nodesNumbers asString;
		cr.
	stringOut := '( 4/' , collectNodesString , ')'.
	^ {stringOut.
	nodesbase}
]

{ #category : #'as yet unclassified' }
LiraFile class >> ke10StringGenNum: num jest: j [
	^ ('10 {1} {2} {3} /' , String crlf) format: { j . num + 1 . num}
]

{ #category : #'as yet unclassified' }
LiraFile class >> ke51StringGenNum: num stiff: s [
	"метод генерит строки для документа 1 а не 3 лиры!!!
для генерации док-та 3 используй метод ke51StringGenStiffNum: num stiff: s
"

	^ ('51 {1} {2} /', String crlf) format: { s . num}
	"s -- номер жесткости
num -- номер узла
"
]

{ #category : #'as yet unclassified' }
LiraFile class >> ke51StringGenStiffNum: num stiff: s [
	^ ('{1} 3 {2} /' , String crlf) format: { num . s}
	"s -- жесткость
num -- номер жесткости
"
]

{ #category : #'as yet unclassified' }
LiraFile class >> nodesStringGenX: x y: y z: z [
	^ (' {1} {2} {3} /' , String crlf) format: { x. y. z}
]

{ #category : #'as yet unclassified' }
LiraFile class >> stemStringStiffGenNumm: i stiff: st [
	^ i asString , ' ' , st
]

{ #category : #initialization }
LiraFile >> initialize [
	super initialize
]

{ #category : #entry }
LiraFile >> startM: m [
	"массив узлов"

	| temp stemStiff czStiff doc1temp doc4temp newLine|
	newLine := '' join: { Character value: 13. Character value: 10 } .
	doc0 := self class doc0.
	doc4temp := self class doc4Mpiles: m.
	doc4 := doc4temp at: 1.
	nodesBase := doc4temp at: 2.
	temp := self class doc3Mpiles: m.
	doc3 := temp at: 3.
	stemStiff := temp at: 1.
	czStiff := temp at: 2.
	doc1temp := self class doc1MpilesM: m stiffStem: stemStiff czStiff: czStiff.
	doc1 := doc1temp at: 1.
	keBase := doc1temp at: 2.
	doc5 := '' join: {'( 5/1 6 /)' . newLine }.
	doc6 := '' join: {'(6/1 0 3 1 1 /)'. newLine} .
	doc7 := '' join: {'( 7/1 0 0 /)'. newLine} .
	doc19 := self class doc19genM: m.
	^ liraDoc := ''
		join:
			{doc0.	doc1.doc3.doc4.	doc5.doc6.doc7.	doc19}
]
