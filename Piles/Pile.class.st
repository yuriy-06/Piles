"
Класс сваи

    Instance Variables
	A:		площадь сваи
	Bp:		условная ширина сваи
	C:		жесткость пружины, вероятно, под концом сваи
	Col:		объект инженерной колонки / скважины
	D:		диаметр или сторона квадрата
	E:		модуль упругости материала сваи
	G1:		осредненный модуль сдвига грунта, прорезаемой сваей толщи
	G2:		осредненный модуль сдвига грунта, на который опирается свая
	Kv:		к-т
	L:		длина сваи
	Lr:		расчетная длина сваи
	Xy:		координаты сваи - Point
	Message   --  для вывода сообщений
	Gcr -- жесткость ствола сваи в формате Лиры в текстовом виде, (вычисляется без порядкового номера)
	Gcv -- то же, но квадратного ствола сваи
	Gamma_cc -- смотри формулу В.5 из приложения В СП Свайные фундаменты
	Rsp  - предельное сопротивление грунта под нижним концом эталонной сваи (необходимо при расчете несущей способности сваи по грунту при наличии данных статического зондирования)
	beta1Rsp - к-т перехода от qs к Rsp (зависит от типа зонда)
	qs - среднее значение сопротивления грунта под наконечником зонда, полученное из опыта,
	на участке 1d выше и 4d ниже острия сваи
	AlfaR - радиус влияния свай, необходимый для вычисления к-в альфа по ф. В.5
"
Class {
	#name : #Pile,
	#superclass : #SelfCheck,
	#instVars : [
		'L',
		'E',
		'A',
		'Kv',
		'G1',
		'G2',
		'C',
		'Call',
		'S',
		'Sadd',
		'B',
		'D',
		'Lr',
		'Bp',
		'Xy',
		'Col',
		'Message',
		'Kv1',
		'Ksi',
		'Lambda1',
		'Alfa',
		'Alfa_k',
		'AlfaR',
		'Beta1',
		'Beta',
		'Gamma_cc',
		'Kmin',
		'I',
		'Ae',
		'Gcr',
		'Gcv',
		'R',
		'Rsp',
		'beta1Rsp',
		'qs'
	],
	#classVars : [
		'KE',
		'Nodes',
		'Stiffness'
	],
	#category : #Piles
}

{ #category : #'as yet unclassified' }
Pile class >> calcPileOne_column: igeCol r: r l: l e: e coord: point gcc: gamma_cc [
	| svaya1 g1 g2 mu1 mu2 col |
	"создается новый объект сваи - и возвращается
	надо сделать проверку на длину сваи

	Gamma_cc -- смотри формулу В.5 из приложения В СП Свайные фундаменты
	
	R для квадратных свай вычисляется как:
	< Pile rForSquared_A: <number> >
	"
	col := igeCol clone.
	svaya1 := Pile new.
	svaya1
		initA: (PileMath squareCircleOfR: r)
			L: l
			e: e
			igeCol: igeCol;
		initPoint: point;
		initGcc: gamma_cc.
	col initL: svaya1 l.
	g1 := col g1.
	g2 := col g2.
	svaya1 initG1: g1 g2: g2; cond.
	mu1 := col mu1.
	mu2 := col mu2.
	svaya1
		kv1: mu1;
		kvmu2: mu2 mu1: mu1;
		ksi;
		lambda1;
		alfa;
		beta1;
		beta;
		c;
		s;
		initCol: col;
		initR: r;
		stiffnessCircleCreate.
	^ svaya1
]

{ #category : #'as yet unclassified' }
Pile class >> calcPileOne_column: igeCol r: r l: l e: e coord: point gcc: gamma_cc alfaR: alfaR [
	| svaya1 g1 g2 mu1 mu2 col |
	"создается новый объект сваи - и возвращается
	надо сделать проверку на длину сваи
	R для квадратных свай вычисляется как:
	< Pile rForSquared_A: <number> >
	"
	col := igeCol clone.
	svaya1 := Pile new.
	svaya1
		initA: r circleSquareOfR
		L: l
		e: e
		igeCol: igeCol.
	svaya1
		initPoint: point;
		initGcc: gamma_cc;
		initAlfaR: alfaR.
	col initL: svaya1 l.
	g1 := col g1.
	g2 := col g2.
	svaya1 initG1: g1 g2: g2; cond.
	mu1 := col mu1.
	mu2 := col mu2.
	svaya1
		kv1: mu1;
		kvmu2: mu2 mu1: mu1;
		ksi;
		lambda1;
		alfa;
		beta1;
		beta;
		c;
		s;
		initCol: col.
	^ svaya1
]

{ #category : #'as yet unclassified' }
Pile class >> initStiffNodKE [
	|val|
	val := OrderedCollection new.
	Stiffness := val.
	KE := val.
	Nodes := val 
]

{ #category : #'as yet unclassified' }
Pile class >> rForKvadr: d [
	| a pi diam r |
	a := d ** 2.
	pi := Float pi.
	diam := 4 * a / pi ** 0.5.
	r := diam / 2.
	^ r
]

{ #category : #getters }
Pile class >> xyAlfaOutM: mPiles [
	^ mPiles collect: [ :each | each xyAlfaOut ].
]

{ #category : #getters }
Pile class >> xyCallOutM: mPiles [
	"массив объектов класса Pile"

	^ mPiles collect: [ :each | each xyCallOut ]
]

{ #category : #calculation }
Pile >> AeEval [
	self
		iEval;
		kMinEval;
		bpEval.
	Ae := Kmin * Bp / (3 * E * I) ** (1 / 5).
	Ae = 0.0
		ifTrue: [ UIManager default alert: 'Kmin = 0.0'.
			Error new
				messageText: 'Kmin = 0.0';
				signal ]
		ifFalse: [ ^ Ae ]
]

{ #category : #getters }
Pile >> CallOut [
	^ Call
]

{ #category : #getters }
Pile >> Gcr [
	^ Gcr
]

{ #category : #getters }
Pile >> Gcv [
	^ Gcv
]

{ #category : #calculation }
Pile >> alfa [
	"метод расчитывающий один из параметров, нужный для вычисления осадки/С одиночной сваи
"

	Alfa := (Kv1 * L / D) ln * 0.17
]

{ #category : #calculation }
Pile >> alfaEval: pile2 [
	"
метод учитывает влияние соседней сваи на к-т К по боковой поверхности сваи
"

	| x1 x2 y1 y2 rk alfa dx dy |
	x1 := self x.
	y1 := self y.
	x2 := pile2 x.
	y2 := pile2 y.
	dx := (x1 - x2) abs.
	dy := (y1 - y2) abs.
	rk := (dx ** 2 + (dy ** 2)) ** 0.5.
	rk > AlfaR
		ifTrue: [ ^ 1 ]
		ifFalse: [ alfa := 1 - (D / rk * (0.789 + (0.229 * dx / rk) - (0.143 * (dx / rk ** 2)))).
			^ alfa ]
]

{ #category : #getters }
Pile >> alfaK [
	^ Alfa_k
]

{ #category : #getters }
Pile >> alfaR [
	^ AlfaR
]

{ #category : #calculation }
Pile >> beta [
	"метод расчитывающий один из параметров, нужный для вычисления осадки/С одиночной сваи
"

	Beta := Beta1/Lambda1 + (0.5*((1 - (Beta1 / Alfa)) / Ksi))
]

{ #category : #calculation }
Pile >> beta1 [
	"метод расчитывающий один из параметров, нужный для вычисления осадки/С одиночной сваи
"

	Beta1 := (Kv * G1 * L / (G2 * D)) ln * 0.17
]

{ #category : #getters }
Pile >> bp [
	^ Bp
]

{ #category : #calculation }
Pile >> bpEval [
	D >= 0.8
		ifTrue: [ Bp := D + 1 ]
		ifFalse: [ Bp := 1.5 * D + 0.5 ].
	Bp := Bp round: 3.
	^ Bp
]

{ #category : #calculation }
Pile >> c [
	C := G1 * L / Beta.
	^ C
]

{ #category : #calculation }
Pile >> cAll [
	"вычисляет откорректированную жесткость под концом сваи  за учет влияния соседних свай"
	Call := 1 / (S + Sadd).
	^ Call
]

{ #category : #calculation }
Pile >> c_ad: pile2 [
	| a x1 y1 x2 y2 dx dy delta k |
	x1 := self x.
	y1 := self y.
	x2 := pile2 x.
	y2 := pile2 y.
	dx := x2 - x1.
	dy := y2 - y1.
	a := (dx ** 2 + (dy ** 2)) ** 0.5.
	k := Kv1 * G1 * L / (2 * G2 * a).
	k > 1
		ifTrue: [ delta := 0.17 * k ln.
			^ G1 * L / delta ]
		ifFalse: [ delta := 0.
			^ 0 ]
]

{ #category : #getters }
Pile >> col [
	^ Col
]

{ #category : #calculation }
Pile >> compareXy: pile2 [
	"возвращает false если равны"

	| x1 x2 y1 y2 |
	x1 := self x.
	y1 := self y.
	x2 := pile2 x.
	y2 := pile2 y.
	x1 = x2
		ifTrue: [ y1 = y2
				ifTrue: [ ^ false ] ].
	^ true
]

{ #category : #calculation }
Pile >> cond [
	| ld g1l g2d s |
	ld := L / D.
	g1l := G1 * L.
	g2d := G2 * D.
	s := g1l / g2d.
	ld > 5
		and: [ s > 1
				ifTrue: [ ^ self ] ].
	Transcript
		show:
			'условие l/d > 5
		G1*L/G2*D > 1 - не выполняется';
		cr;
		show: 'свая с координатами ';
		show: self xy;
		cr;
		show: 'G1, G2, L, l/d = ';
		show: #(G1 G2 L ld)
]

{ #category : #getters }
Pile >> g1 [
^ G1
]

{ #category : #calculation }
Pile >> iEval [
	"для случая круглых свай"

	I := 3.14159 * (D / 2 ** 4) / 4.
	^ I
]

{ #category : #calculation }
Pile >> initA: a L: l e: e igeCol: igCol [
	A := a.
	L :=l.
	D := ((A * 4) / 3.14159 ) sqrt .
	E := e.
	Message := ''
]

{ #category : #setters }
Pile >> initAlfaK: alfa_k [
Alfa_k := alfa_k * Gamma_cc 
"Gamma_cc -- смотри формулу В.5 из приложения В СП Свайные фундаменты"
]

{ #category : #setters }
Pile >> initAlfaR: r [
AlfaR := r
]

{ #category : #setters }
Pile >> initCol: col [
Col := col.
^ self
]

{ #category : #getters }
Pile >> initG1: g1 g2: g2 [
	G1 := g1.
	G2 := g2
]

{ #category : #setters }
Pile >> initGcc: gc [
Gamma_cc := gc
]

{ #category : #setters }
Pile >> initGcr: string [
Gcr := string .
^ self
]

{ #category : #getters }
Pile >> initPoint: point [
	Xy := point
]

{ #category : #setters }
Pile >> initR: r [
R := r.
^ self
]

{ #category : #setters }
Pile >> initSadd: sad [
Sadd := sad
]

{ #category : #initialization }
Pile >> initialize [
	super initialize.
	Gcr := ''.	"жесткости круглой и квадратной, свай (по-умолчанию неизвестно, какая свая)"
	Gcv := ''
]

{ #category : #calculation }
Pile >> kMinEval [
	Kmin := Col kMinEval.
	^ Kmin
]

{ #category : #calculation }
Pile >> ksi [
	Ksi := E * A / (G1 * L squared)
]

{ #category : #calculation }
Pile >> kv1: mu1 [
	Kv1 := self kvCommon: mu1
]

{ #category : #calculation }
Pile >> kvCommon: mu [
	^ 2.82 - (3.78 * mu) + (2.18 * mu squared)
]

{ #category : #calculation }
Pile >> kvmu2: mu2 mu1: mu1 [
	| mu |
	mu := (mu2 + mu1) / 2.
	Kv := self kvCommon: mu
]

{ #category : #getters }
Pile >> l [
^ L
]

{ #category : #calculation }
Pile >> lambda1 [
	| k |
	k := Ksi ** (3 / 4).
	Lambda1 := 2.12 * k / (1 + (2.12 * k))
]

{ #category : #calculation }
Pile >> lrEval [
	self AeEval.
	Lr := 2 / Ae round: 3.
	^ Lr
]

{ #category : #calculation }
Pile >> s [
	S :=  Beta / (G1 * L) .
	^ S
]

{ #category : #calculation }
Pile >> s_ad: pile2 [
	"дополнительная осадка от одиночной нагрузки
	вообще говоря, будет видимо, зависеть от геологии в точке влияющей сваи,
	поэтому надо бы привязывать колонки к конкретным сваям
	"

	| a x1 y1 x2 y2 dx dy delta k |
	x1 := self x.
	y1 := self y.
	x2 := pile2 x.
	y2 := pile2 y.
	dx := x2 - x1.
	dy := y2 - y1.
	a := (dx ** 2 + (dy ** 2)) ** 0.5.
	k := Kv1 * G1 * L / (2 * G2 * a).
	k > 1
		ifTrue: [ delta := 0.17 * k ln.
			^ delta / (G1 * L) ]
		ifFalse: [ delta := 0.
			^ 0 ]
]

{ #category : #setters }
Pile >> setBeta1Rsp: b [
	beta1Rsp := b
]

{ #category : #setters }
Pile >> setQs: q [
	qs := q
]

{ #category : #getters }
Pile >> stemStiffness [
	self Gcv = ''
		ifTrue: [ ^ self Gcr ]
		ifFalse: [ ^ self Gcv ]
]

{ #category : #calculation }
Pile >> stiffnessCircleCreate [

	"жесткость вычисляется без порядкового номера"
	
	|f|
	f := ((R squared) * 3.14 * 2.5) round: 4. "вычисляем линейную плотность "
	Gcr := '' join:  {'S6 3.000E+006 '. ((R * 2 * 100) round: 4) . ' 0/
	'. '0 RO '. f . '/
	'}.
	^ Gcr
]

{ #category : #getters }
Pile >> x [
	^ Xy x
]

{ #category : #getters }
Pile >> xy [
	^ Xy
]

{ #category : #getters }
Pile >> xyAlfaOut [
	| x y a |
	x := self x.
	y := self y.
	a := Alfa_k round: 3.	"выведем округленное значение"
	^ {  x .  y. a }
]

{ #category : #getters }
Pile >> xyCallOut [
	| x y c |
	x := self x.
	y := self y.
	c := Call round: 1.	"выведем округленное значение"
	^  {  x .  y .  c } .
]

{ #category : #getters }
Pile >> xyPoint: point [
	Xy := point
]

{ #category : #getters }
Pile >> y [
	^ Xy y
]
