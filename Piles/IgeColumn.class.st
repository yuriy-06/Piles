"
Класс инженерной колонки, содержит в себе список грунтов.

    Instance Variables
	IgeList:		каждый член является объектом класса Ige

"
Class {
	#name : #IgeColumn,
	#superclass : #Object,
	#instVars : [
		'IgeList',
		'G1',
		'G2',
		'Mu1',
		'Mu2',
		'Kmin',
		'name'
	],
	#category : #Piles
}

{ #category : #errorsChecking }
IgeColumn class >> checkingE: e [
	e isNil
		ifFalse: [ e < 50
				ifTrue: [ UIManager default
						alert:
							'Т.к. модуль упругости сваи мы задаем в тс/м2, то и модуль упругости
надо задавать в тс/м2, иначе осадки вычисляются в 100 раз меньше,
далее добро пожаловать в отладчик'.
					Error new
						messageText: 'Неправильна размерность модуля упругости';
						signal ] ]
		ifTrue: [ UIManager default alert: 'чтобы посчитать осадку сваи, нужны модули упругости грунтов в базе'.
			Error new
				messageText: 'Отсутствует модуль упругости';
				signal ]
]

{ #category : #errorsChecking }
IgeColumn class >> checkingIgeList: igeList [
	igeList = OrderedCollection new
		ifFalse: [ ^ self ].
	UIManager default
		alert:
			'в методе /evalGn_mIge: igeList/ класса igeColumn параметр igeList = #()'.
	Error new
		messageText: 'igeList = #()';
		signal
]

{ #category : #'as yet unclassified' }
IgeColumn class >> cutSoleIgeList: igesList hZagl: hZagl [
	| out z1 |
	"отрезает грунты ниже подошвы"
	z1 := 0.
	out := igesList
		collect: [ :ige | 
			| z2 ot |
			z2 := z1 + ige h.
			(z2 > hZagl and: z1 >= hZagl)
				ifTrue: [ ot := ige ].
			(z2 > hZagl and: z1 < hZagl)
				ifTrue: [ | len |
					len := z2 - hZagl.
					ot := ige cloneH: len ].
			z1 := z1 + ige h.
			ot ].
	^ out select: [ :elem | elem isNotNil ]
]

{ #category : #errorsChecking }
IgeColumn class >> igeListChecking: igeList [
	igeList size = 1
		ifTrue: [ Transcript show: 'в колонке всего один слой? см. IgeColumn initL: ' ]
]

{ #category : #setters }
IgeColumn class >> initList: igeList [
	^ (IgeColumn new) initList: igeList
]

{ #category : #calculation }
IgeColumn >> evalGn_mIge: igeList [
	| h gh |
	self class checkingIgeList: igeList.
	h := 0.
	gh := 0.
	igeList
		do: [ :elem | 
			h := h + elem h.
			self class checkingE: elem e.
			gh := gh + (elem h * (elem e * 0.4)) ].
	^ gh / h
]

{ #category : #calculation }
IgeColumn >> evalMun_mIge: igeList [
	| h mh |
	h := 0.
	mh := 0.
	igeList
		do: [ :elem | 
			h := h + elem h.
			elem mu isNil
				ifTrue: [ UIManager default
						alert:
							'Чтобы посчитать осадку сваи, 
нужны значения к-та поперечной деформации (Пуассона) в базе'.
					Error new
						messageText: 'Отсутствует к-т Пуассона в базе';
						signal ].
			mh := mh + (elem h * elem mu) ].
	^ mh / h
]

{ #category : #getters }
IgeColumn >> g1 [
	^ G1
]

{ #category : #getters }
IgeColumn >> g2 [
	^ G2
]

{ #category : #modifying }
IgeColumn >> igeListExtendL: l3 [
	"продлевает последний ИГЭ на необходимую глубину, если надо"

	| last |
	last := self igelistOut last.
	last z2 < l3
		ifTrue: [ last initZ2: l3 ].
	^ self
]

{ #category : #getters }
IgeColumn >> igelistOut [
	^ IgeList
]

{ #category : #settersModifCalc }
IgeColumn >> initL: l [
	| l1 l2 l3 inth1 inth2 g1m g2m |
	l1 := 0.
	l2 := l.
	l3 := l * 1.5.
	IgeList do: [ :elem | l1 := elem initZ1: l1 ].	"здесь определили все координаты"
	self igeListExtendL: l3.
	self split_L2: l2 l3: l3 .
	"закончили резку грунтов, пора получать осредненные хар-ки"
	inth1 := IntervalH new.
	inth1 initZ1: 0 z2: l.
	inth2 := IntervalH new.
	inth2 initZ1: l z2: l3.
	g1m := IgeList select: [ :elem | inth1 includesIge: elem ].
	g2m := IgeList select: [ :elem | inth2 includesIge: elem ].
	"разбили на 2 массива грунтов, теперь пора получить осредненные хар-ки"
	G1 := self evalGn_mIge: g1m.
	G2 := self evalGn_mIge: g2m.
	Mu1 := self evalMun_mIge: g1m.
	Mu2 := self evalMun_mIge: g2m
]

{ #category : #setters }
IgeColumn >> initList: igelist [
	IgeList := igelist 
]

{ #category : #calculation }
IgeColumn >> kMinEval [
	^ Kmin := (IgeList collect: [ :each | each k ]) min 
	
]

{ #category : #getters }
IgeColumn >> mu1 [
	^ Mu1
]

{ #category : #getters }
IgeColumn >> mu2 [
	^ Mu2
]

{ #category : #setters }
IgeColumn >> setName: arg1 [
	name := arg1
]

{ #category : #modifying }
IgeColumn >> split_L2: l2 l3: l3 [
	| igeListSplitter |
	igeListSplitter := [ :z | 
	| mass |
	mass := OrderedCollection new.	"в него помещаем все объекты грунтов, порезаные с каким-то шагом м"
	IgeList do: [ :elem | (elem splitterZ: z) do: [ :e | mass add: e ] ].
	IgeList := mass ].
	self splitter1m.	"порезали через 1 м"
	igeListSplitter value: l2.
	igeListSplitter value: l3
]

{ #category : #modifying }
IgeColumn >> splitter1m [
	| m |
	m := OrderedCollection new.
	IgeList do: [ :elem | elem splitterZmHsplit do: [ :e | m add: e ] ].	"порезали через 1 м"
	IgeList := m.
	^ IgeList
]

{ #category : #modifying }
IgeColumn >> splitterZ: z [
|m|
	m := OrderedCollection new.
	IgeList do: [ :elem | (elem splitterZ: z) do: [ :each | m add: each ] ] .
	IgeList := m.
	^ m
]

{ #category : #modifying }
IgeColumn >> zFactFictiv [
	| z |
	z := 0.
	IgeList := IgeList
		collect: [ :ige | 
			ige
				setZ1Fact: z;
				setZ2Fact: z + ige h.
			z := z + ige h.
			ige ]
]
