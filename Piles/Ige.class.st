"
Класс инженерного грунта

    Instance Variables
	E:		модуль упругости грунта
	H:		мощность слоя
	Mu:		к-т пуассона
	Z1:		координата
	Z2:		координата
	K:   		к-т пропорциональности грунта, окружающего сваю, т.к. расчеты в Лире мы выполняем в тс, то задавать в тс/м4
	hSplit -	величина разрезкики ИГЭ для задания КЕ свай,
		по умолчанию при инициализации устанавливается в 1м,
		если не будет впоследствии переопределен методом initHsplit
	zondType - тип грунта для зондирования (1 - песчаный, 2 - глинистый)
	
	далее идут только параметры, необходимые для расчета сопротивления по боковой поверхности свай
	igeType - 1 или 2 (песчаный или глинистый)
	size	-	крупность (только для песков)
	subsidence -	учет негативного трения (знак фи по боковой поверхности), но вообще, может учитываться как поправочный к-т, на любой фактор (например, можно занлить fi для насыпного грунта)
		t7_7a1 t7_7a2 t7_7a3 t7_7a4  -   необходимы для расчета R буроинъекционных свай
		phi - угол внутреннего трения, для расчета буроинъекционных свай
		a1 a2 a3 a4  -  см формулу 7.12 СП Свайные
		gamma_s   плотность частиц грунта (кН/м3, для расчета буроинъекционных свай)
		gamma     плотность грунта
		fiAbs     fi без поправок gamma_r_f
		density - учитывает плотность песков (1 - плотный), а также плотность супесей и суглиноков (1 - e < 0.5), и плотность глин (1 - e < 0.6)
			при расчете сопротивления по боковой поверхности сваи
		silt - учитывает определенным способом негативное трение
"
Class {
	#name : #Ige,
	#superclass : #Object,
	#instVars : [
		'H',
		'E',
		'Mu',
		'Z1',
		'Z2',
		'K',
		'Message',
		'Name',
		'description',
		'Cz',
		'hSplit',
		'zondType',
		'igeType',
		'il',
		'il_suppose',
		'size',
		'subsidence',
		't7_3',
		'zSr',
		'fi',
		'closeSand',
		'fi1',
		'fi2',
		'gamma_r_f',
		't7_7a1',
		't7_7a2',
		't7_7a3',
		't7_7a4',
		'pfi',
		'a1',
		'a2',
		'a3',
		'a4',
		'gamma',
		'gamma_s',
		'pfi_I',
		'z1Fact',
		'z2Fact',
		'z1Range',
		'z2Range',
		'fiAbs',
		'density',
		'silt'
	],
	#category : #Piles
}

{ #category : #'as yet unclassified' }
Ige class >> cloneZ1: z1 z2: z2 ig: ig [
	| o |
	"клонирует объект грунта со всеми хар-ми, кроме координат, которые переназначаются"
	o := ig clone.
	o initZ1: z1 z2: z2.
	o initH.
	^ o
]

{ #category : #'as yet unclassified' }
Ige class >> createFromArray: array [
	| s |
	s := Ige new.
	s
		initName: (array at: 1)
		e: (array at: 2)
		mu: (array at: 3)
		k: (array at: 4).
	^ s
]

{ #category : #'as yet unclassified' }
Ige class >> createInitName: nameM desc: desc e: eM mu: muM k: kM [
	| s |
	s := Ige new.
	s
		initName: nameM
		desc: desc
		e: eM
		mu: muM
		k: kM.
	^ s
]

{ #category : #'as yet unclassified' }
Ige class >> createInitName: nameM e: eM mu: muM k: kM [
	| s |
	s := Ige new.
	s
		initName: nameM
		e: eM
		mu: muM
		k: kM.
	^ s
]

{ #category : #calculationsSetters }
Ige >> CzEval [
	| z |
	z := (Z1 + Z2) / 2.
	K isNil ifTrue: [ UIManager default alert: 'В базе надо задавать К для генерации комп. модели свай' .
		Error new messageText: 'Отсутствует K'; signal] .
	Cz := (K / 1 * z) asInteger. "согласно изм. 2 к СП Свайные фундаменты, gamma_cz стало не 3, а 1"
	^ Cz
]

{ #category : #calculationsSetters }
Ige >> aEval_hd: hd d: d [
	| xList valRange pfiVal pfi1range pfi2range ym xm aBlock aI aII hdList hdVal y1m y2m hd1 hd2 dVal d1 d2 yArrayFromDict |
	xList := #(#(23 25) #(25 27) #(27 29) #(29 31) #(31 33) #(33 35) #(35 37) #(37 39)).	"массив, нужный для вычисления соотв. интервала"
	pfiVal := pfi.
	pfi < 23
		ifTrue: [ pfiVal := 23.0 ].
	pfi > 39
		ifTrue: [ pfiVal := 39.0 ].
	valRange := NPiles intList: xList val: pfiVal.	"определили глубины интервала для интерполяции по табл. 7.7"
	pfi1range := valRange at: 1.
	pfi2range := valRange at: 2.
	xm := {pfi1range.
	pfi2range}.
	aBlock := [ :range :table | 
	| val |
	[ val := table at: range asString ]
		on: KeyNotFound
		do: [ :ex | val := table at: range ].
	val ].
	ym := {(aBlock value: pfi1range value: t7_7a1).
	(aBlock value: pfi2range value: t7_7a1)}.
	a1 := Interpolation int1d_xm: xm ym: ym x: pfiVal.
	ym := {(aBlock value: pfi1range value: t7_7a2).
	(aBlock value: pfi2range value: t7_7a2)}.
	a2 := Interpolation int1d_xm: xm ym: ym x: pfiVal.
	hdList := #(#(4.0 5.0) #(5.0 7.5) #(7.5 10.0) #(10.0 12.5) #(12.5 15.0) #(15.0 17.5) #(17.5 20.0) #(20.0 22.5) #(22.5 25.0)).
	hdVal := hd.
	hd < 4
		ifTrue: [ hdVal := 4.0 ].
	hd > 25
		ifTrue: [ hdVal := 25.0 ].
	valRange := NPiles intList: hdList val: hdVal.
	hd1 := valRange at: 1.
	hd2 := valRange at: 2.
	yArrayFromDict := [ :dict | 
	| y1 y2 |
	y1 := aBlock value: pfi1range value: dict.
	y2 := aBlock value: pfi2range value: dict.
	{y1.
	y2} ].
	y1m := aBlock value: hd1 value: t7_7a3.
	y1m := yArrayFromDict value: y1m.
	y2m := aBlock value: hd2 value: t7_7a3.
	y2m := yArrayFromDict value: y2m.
	aI := Interpolation int1d_xm: xm ym: y1m x: pfiVal.
	aII := Interpolation int1d_xm: xm ym: y2m x: pfiVal.
	a3 := Interpolation
		int1d_xm:
			{hd1.
			hd2}
		ym:
			{aI.
			aII}
		x: hdVal.
	dVal := d.
	d < 0.8
		ifTrue: [ dVal := 0.8 ].
	d > 4.0
		ifTrue: [ dVal := 4.0 ].
	d1 := 0.8.
	d2 := 4.0.
	y1m := aBlock value: d1 value: t7_7a4.
	y1m := yArrayFromDict value: y1m.
	y2m := aBlock value: d2 value: t7_7a4.
	y2m := yArrayFromDict value: y2m.
	aI := Interpolation int1d_xm: xm ym: y1m x: pfiVal.
	aII := Interpolation int1d_xm: xm ym: y2m x: pfiVal .
	a4 := Interpolation
		int1d_xm:
			{d1.
			d2}
		ym:
			{aI.
			aII}
		x: dVal.
	^ {a1.
	a2.
	a3.
	a4}
]

{ #category : #modifications }
Ige >> cloneH: h [
	| o |
	"клонирует объект грунта со всеми хар-ми, кроме H, которые переназначаются"
	o := self clone.
	o setH: h.
	^ o
]

{ #category : #modifications }
Ige >> cloneZ1: z1 z2: z2 ig: ig [
	| o |
	"клонирует объект грунта со всеми хар-ми, кроме координат, которые переназначаются"
	o := ig clone.
	o initZ1: z1 z2: z2.
	o initH.
	^ o
]

{ #category : #getters }
Ige >> cz [
	^ Cz
]

{ #category : #getters }
Ige >> e [
	^ E
]

{ #category : #getters }
Ige >> fi [
	^ fi
]

{ #category : #getters }
Ige >> fiAbs [
	^ fiAbs
]

{ #category : #calculationsSetters }
Ige >> fiEval [
	| xList valRange |
	xList := #(#(1 2) #(2 3) #(3 4) #(4 5) #(5 6) #(6 8) #(8 10) #(10 15) #(15 20) #(20 25) #(25 30) #(30 35) #(35 40)).	"массив, нужный для вычисления соотв. интервала"
	zSr := (Z1 + Z2) / 2.
	zSr < 1
		ifTrue: [ fi := 0 . fiAbs := 0 . ^ self ].
	valRange := NPiles intList: xList val: zSr.	"определили глубины интервала для интерполяции по табл. 7.2 и 7.3
		если zSr < 1 - возникает ошибка, т.к. мы не попадаем в интервал"
	z1Range := valRange at: 1.
	z2Range := valRange at: 2.
	igeType = 1
		ifTrue: [ self fiType1 ]
		ifFalse: [ self fiType2 ].
	^ self
]

{ #category : #calculationsSetters }
Ige >> fiType1 [
	| ym sandBlock sizeIge cl |
	cl := density = 1
		ifTrue: [ 1.3 ]
		ifFalse: [ 1 ]. "см. прим. 3 к табл. 7.3"
	size = 'гравелистый'
		ifTrue: [ ^ fi := 0 ].
	size = 'крупный'
		ifTrue: [ sizeIge := #'0.2' ].
	size = 'средней крупности'
		ifTrue: [ sizeIge := #'0.2' ].
	size = 'мелкий'
		ifTrue: [ sizeIge := #'0.3' ].
	size = 'пылеватый'
		ifTrue: [ sizeIge := #'0.4' ].
	sandBlock := [ :zRange | 
	| val |
	val := (t7_3 at: zRange) at: sizeIge.
	val ].
	ym := {(sandBlock value: z1Range).
	(sandBlock value: z2Range)}.
	fi := Interpolation
		int1d_xm:
			{z1Range.
			z2Range}
		ym: ym
		x: zSr.
	fiAbs := fi * cl * subsidence.
	fi := fi * cl * subsidence * gamma_r_f.
	^ fi
]

{ #category : #calculationsSetters }
Ige >> fiType2 [
	| int valRange il1 il2 ym1 ym2 clayBlock ilValue cl|
	int := #(#(0.2 0.3) #(0.3 0.4) #(0.4 0.5) #(0.5 0.6) #(0.6 0.7) #(0.7 0.8) #(0.8 0.9) #(0.9 1)).
	"если показатель текучести по геологии неизвестен,
	то используем предполагаемый показатель"
	((il isNil) and: (il_suppose isNil))
		ifTrue: [ UIManager default alert:
					'оба значения il и il_suppose не заданы для глинистого грунта,
		что приводит к ошибке выполнения программы' . Error new messageText: 'Нет данных по Il, Il_suppose'; signal].
	ilValue := il isNil
		ifTrue: [ il_suppose ]
		ifFalse: [ il ].
	valRange := ilValue < 0.2
		ifTrue: [ #(0.2 0.3) ]
		ifFalse: [ NPiles intList: int val: ilValue ].	"здесь предусмотреть выход за максимальное значение Il"
	il1 := (valRange at: 1) asString.
	il2 := (valRange at: 2) asString.
	clayBlock := [ :zRange :ilv | 
	| val |
	val := (t7_3 at: zRange) at: ilv.
	val ].
	ym1 := {(clayBlock value: z1Range value: il1).
	(clayBlock value: z1Range value: il2)}.
	ym2 := {(clayBlock value: z2Range value: il1).
	(clayBlock value: z2Range value: il2)}.
	il1 := il1 asNumber.
	il2 := il2 asNumber.
	fi1 := Interpolation
		int1d_xm:
			{il1.
			il2}
		ym: ym1
		x: ilValue.
	fi2 := Interpolation
		int1d_xm:
			{il1.
			il2}
		ym: ym2
		x: ilValue.
	fi := Interpolation
		int1d_xm:
			{z1Range.
			z2Range}
		ym:
			{fi1.
			fi2}
		x: zSr.
	cl := density = 1
		ifTrue: [ 1.15 ]
		ifFalse: [ 1 ]. "см. прим. 4 к табл. 7.3"
	subsidence = nil ifTrue: [ UIManager default alert: 'Задай в базе subsidence (1 - по умолчанию, для негативного трения - другое)' . Error new messageText: 'Erroe subsidance input'; signal] .
	fiAbs := fi * subsidence * cl .
	fi := fi * subsidence * cl * gamma_r_f.
	silt isNotNil ifTrue: [ fi := -5 * gamma_r_f . fiAbs := -5 ] .
	^ fi
]

{ #category : #getters }
Ige >> h [
	^ H
]

{ #category : #getters }
Ige >> hSplit [
	^ hSplit
]

{ #category : #calculationsSetters }
Ige >> initH [
	H := Z2 - Z1
]

{ #category : #setters }
Ige >> initHsplit: h [
	"по умолчанию, при создании объекта этот параметр равен 1м"

	hSplit := h.
	^ self
]

{ #category : #otherSetters }
Ige >> initName: name desc: desc e: e mu: mu k: k [
	Name := name.
	E := e.
	Mu := mu.
	K := k.
	description := desc
]

{ #category : #otherSetters }
Ige >> initName: name e: e mu: mu k: k [
	Name := name.
	E := e.
	Mu := mu.
	K := k
]

{ #category : #otherSetters }
Ige >> initName: name h: h e: e mu: mu k: k [
	H := h.
	Name := name.
	E := e.
	Mu := mu.
	K := k
]

{ #category : #otherSetters }
Ige >> initZ1: z1 [
	Z1 := z1.
	Z2 := z1 + H.
	self CzEval.
	^ Z2
]

{ #category : #otherSetters }
Ige >> initZ1: z1 z2: z2 [
	Z1 := z1.
	Z2 := z2.
	self CzEval
]

{ #category : #otherSetters }
Ige >> initZ2: z2 [
	Z2 := z2.
	H := Z2 - Z1.
	^ self
]

{ #category : #initialization }
Ige >> initialize [
	super initialize.
	hSplit := 1.
	self t7_3_init .
	self t7_7_init 
]

{ #category : #getters }
Ige >> k [
	^ K
]

{ #category : #getters }
Ige >> mu [
	^ Mu
]

{ #category : #getters }
Ige >> name [
	^ Name
]

{ #category : #setters }
Ige >> setCloseSand: s [
	closeSand := s
]

{ #category : #setters }
Ige >> setDensity: val [
	density := val
]

{ #category : #setters }
Ige >> setGamma: gam [
	"просто плотность"

	gamma := gam
]

{ #category : #setters }
Ige >> setGammaRf: g [
	gamma_r_f := g
]

{ #category : #setters }
Ige >> setGamma_s: gam [
	"плотность частиц грунта"

	gamma_s := gam
]

{ #category : #setters }
Ige >> setH: hv [
	H := hv
]

{ #category : #setters }
Ige >> setIlSuppose: ilv [
	il_suppose  := ilv
]

{ #category : #setters }
Ige >> setK: k [
	K := k
]

{ #category : #setters }
Ige >> setName: n [
	Name := n
]

{ #category : #setters }
Ige >> setPhi: val [
	pfi := val
]

{ #category : #setters }
Ige >> setPhi_I: val [
	pfi_I := val
]

{ #category : #setters }
Ige >> setSilt: v [
	silt := v
]

{ #category : #setters }
Ige >> setZ1: z [
	Z1 := z
]

{ #category : #setters }
Ige >> setZ1Fact: z [
	z1Fact := z
]

{ #category : #setters }
Ige >> setZ2: z [
	Z2 := z
]

{ #category : #setters }
Ige >> setZ2Fact: z [
	z2Fact := z
]

{ #category : #setters }
Ige >> set_igeType: type il: ilv size: sz subsidence: sbv [
	igeType := type.
	il := ilv.
	size := sz.
	subsidence := sbv
]

{ #category : #modifications }
Ige >> splitterZ: z [
|i1 i2 |
"разрезает слой по конкретной координате, если такая имеется в пределах высоты слоя"
((Z1 < z) and: (z < Z2)) ifTrue: [ 
		i1 := self class cloneZ1: Z1 z2: z ig: self .
		i2 := self class  cloneZ1: z z2: Z2 ig: self .
		z1Fact := self z1Fact. z2Fact := self z2Fact.
		i1 setZ2Fact: z1Fact - (z - Z1).
		i2 setZ1Fact: z1Fact - (z - Z1).
		^ { i1 . i2 }
		].
^ { self }
]

{ #category : #modifications }
Ige >> splitterZmHsplit [
	| count sizeIter m |
	"если слой больше 1 м, то режет его на соотв. число слоев"
	H < hSplit
		ifTrue: [ ^ { self } ].
	count := Z1.
	sizeIter := H // hSplit. "число итераций, проверить чтоб последний слой не был нулевым"
	m := (1 to: sizeIter)
		collect: [ :e | |elem| 
			elem := self class cloneZ1: count z2: count + hSplit ig: self.
			elem setZ2Fact: z1Fact - hSplit . 
			elem setZ1Fact: z1Fact .
			z1Fact := z1Fact - hSplit .
			count := count + hSplit . 
			elem].
	m := m asOrderedCollection .
	Z2 = count
		ifFalse: [ m add: ((self class cloneZ1: count z2: Z2 ig: self) setZ1Fact: z1Fact) ].
	^ m
]

{ #category : #initialization }
Ige >> t7_3 [
	^ t7_3 
]

{ #category : #initialization }
Ige >> t7_3_init [
	| blockInit |
	t7_3 := Dictionary new.
	blockInit := [ :z :list | 
	t7_3
		at: z
		put:
			(Dictionary
				newFrom:
					{#'0.2' -> (list at: 1).
					#'0.3' -> (list at: 2).
					#'0.4' -> (list at: 3).
					#'0.5' -> (list at: 4).
					#'0.6' -> (list at: 5).
					#'0.7' -> (list at: 6).
					#'0.8' -> (list at: 7).
					#'0.9' -> (list at: 8).
					#'1' -> (list at: 9)}) ].
	blockInit value: 1 value: #(35 23 15 12 8 4 4 3 2).
	blockInit value: 2 value: #(42 30 21 17 12 7 5 4 4).
	blockInit value: 3 value: #(48 35 25 20 14 8 7 6 5).
	blockInit value: 4 value: #(53 38 27 22 16 9 8 7 5).
	blockInit value: 5 value: #(56 40 29 24 17 10 8 7 6).
	blockInit value: 6 value: #(58 42 31 25 18 10 8 7 6).
	blockInit value: 8 value: #(62 44 33 26 19 10 8 7 6).
	blockInit value: 10 value: #(65 46 34 27 19 10 8 7 6).
	blockInit value: 15 value: #(72 51 38 28 20 11 8 7 6).
	blockInit value: 20 value: #(79 56 41 30 20 12 8 7 6).
	blockInit value: 25 value: #(86 61 44 32 20 12 8 7 6).
	blockInit value: 30 value: #(93 66 47 34 21 12 9 8 7).
	blockInit value: 35 value: #(100 70 50 36 22 13 9 8 7).
	blockInit value: 40 value: #(107 74 53 38 23 14 9 8 7)
]

{ #category : #initialization }
Ige >> t7_7_init [
	"необходим для расчета буроинъекционных свай"
	t7_7a1 := Dictionary newFrom: 
	{#'23' -> 9.5. 
	#'25' -> 12.6.
	#'27' -> 17.3.
	#'29' -> 24.4.
	#'31' -> 34.6.
	#'33' -> 48.6.
	#'35' -> 71.3.
	#'37' -> 108.0.
	#'39' -> 163.0
	}.
	t7_7a2 := Dictionary newFrom: 
	{#'23' -> 18.6. 
	#'25' -> 24.8.
	#'27' -> 32.8.
	#'29' -> 45.5.
	#'31' -> 64.0.
	#'33' -> 87.6.
	#'35' -> 127.0.
	#'37' -> 185.0.
	#'39' -> 260.0
	}.
	t7_7a3 := Dictionary new.
	t7_7a3
		at: 4.0 put: (Dictionary newFrom:
	{#'23' -> 0.78. 
	#'25' -> 0.79.
	#'27' -> 0.8.
	#'29' -> 0.82.
	#'31' -> 0.84.
	#'33' -> 0.85.
	#'35' -> 0.85.
	#'37' -> 0.85.
	#'39' -> 0.87
	});
	at: 5.0 put: (Dictionary newFrom:
	{#'23' -> 0.75. 
	#'25' -> 0.76.
	#'27' -> 0.77.
	#'29' -> 0.79.
	#'31' -> 0.81.
	#'33' -> 0.82.
	#'35' -> 0.83.
	#'37' -> 0.84.
	#'39' -> 0.85
	});
	at: 7.5 put: (Dictionary newFrom:
	{#'23' -> 0.68. 
	#'25' -> 0.7.
	#'27' -> 0.71.
	#'29' -> 0.74.
	#'31' -> 0.76.
	#'33' -> 0.78.
	#'35' -> 0.8.
	#'37' -> 0.82.
	#'39' -> 0.84
	});
	at: 10.0 put: (Dictionary newFrom:
	{#'23' -> 0.62. 
	#'25' -> 0.65.
	#'27' -> 0.67.
	#'29' -> 0.7.
	#'31' -> 0.73.
	#'33' -> 0.75.
	#'35' -> 0.77.
	#'37' -> 0.79.
	#'39' -> 0.81
	});
	at: 12.5 put: (Dictionary newFrom:
	{#'23' -> 0.58. 
	#'25' -> 0.61.
	#'27' -> 0.63.
	#'29' -> 0.67.
	#'31' -> 0.7.
	#'33' -> 0.73.
	#'35' -> 0.75.
	#'37' -> 0.78.
	#'39' -> 0.8
	});
	at: 15.0 put: (Dictionary newFrom:
	{#'23' -> 0.55. 
	#'25' -> 0.58.
	#'27' -> 0.61.
	#'29' -> 0.65.
	#'31' -> 0.68.
	#'33' -> 0.71.
	#'35' -> 0.73.
	#'37' -> 0.76.
	#'39' -> 0.79
	});
	at: 17.5 put: (Dictionary newFrom:
	{#'23' -> 0.51. 
	#'25' -> 0.55.
	#'27' -> 0.58.
	#'29' -> 0.62.
	#'31' -> 0.66.
	#'33' -> 0.69.
	#'35' -> 0.72.
	#'37' -> 0.75.
	#'39' -> 0.78
	});
	at: 20.0 put: (Dictionary newFrom:
	{#'23' -> 0.49. 
	#'25' -> 0.53.
	#'27' -> 0.57.
	#'29' -> 0.61.
	#'31' -> 0.65.
	#'33' -> 0.68.
	#'35' -> 0.72.
	#'37' -> 0.75.
	#'39' -> 0.78
	});
	at: 22.5 put: (Dictionary newFrom:
	{#'23' -> 0.46. 
	#'25' -> 0.51.
	#'27' -> 0.55.
	#'29' -> 0.6.
	#'31' -> 0.64.
	#'33' -> 0.67.
	#'35' -> 0.71.
	#'37' -> 0.74.
	#'39' -> 0.77
	});
	at: 25.0 put: (Dictionary newFrom:
	{#'23' -> 0.44. 
	#'25' -> 0.49.
	#'27' -> 0.54.
	#'29' -> 0.59.
	#'31' -> 0.63.
	#'33' -> 0.67.
	#'35' -> 0.7.
	#'37' -> 0.74.
	#'39' -> 0.77
	}).
	t7_7a4 := Dictionary new.
	t7_7a4  at: 0.8 put: (Dictionary newFrom:
	{#'23' -> 0.34. 
	#'25' -> 0.31.
	#'27' -> 0.29.
	#'29' -> 0.27.
	#'31' -> 0.26.
	#'33' -> 0.25.
	#'35' -> 0.24.
	#'37' -> 0.23.
	#'39' -> 0.22
	});
	at: 4.0 put: (Dictionary newFrom:
	{#'23' -> 0.25. 
	#'25' -> 0.24.
	#'27' -> 0.23.
	#'29' -> 0.22.
	#'31' -> 0.21.
	#'33' -> 0.2.
	#'35' -> 0.19.
	#'37' -> 0.18.
	#'39' -> 0.17
	}).
	
]

{ #category : #getters }
Ige >> type [
	^ igeType 
]

{ #category : #getters }
Ige >> z1 [
	^ Z1
]

{ #category : #getters }
Ige >> z1Fact [
	^ z1Fact
]

{ #category : #getters }
Ige >> z2 [
	^ Z2
]

{ #category : #getters }
Ige >> z2Fact [
	^ z2Fact
]

{ #category : #getters }
Ige >> zSr [
	^ (Z1 + Z2)/2
]
