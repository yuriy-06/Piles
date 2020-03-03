"
Здесь будет основная логика программы

запуск программы:

создали обект с данными в методе dataPile
g := StartPile new.
запустили вычисления и получили массив с данными
m := g startPileR: 5 .

или так:
start := StartPile new.
m := start startPileR: 1.5 dataPilesM: (start dataPileCsvNoName: 'nodes.csv').

l := LiraFile new.  --  создали объект, генерирующий комп. модель свай
ll := l startM: m.    
ll - сгенерированная комп. модель ввиде строки текста
также можно делать inspect для m и l .

более коротко и правильно:
g := StartPile new.
m := g startPileR: 2.17 dataPilesM: g dataPileCsv3983_2_2 .
l := LiraFile new.
ll := l startM: m.
вывести в файл потом так:
f := FileStream newFileNamed: 'piles.txt'.
f nextPutAll: ll.
f close.

надо дописать для квадратных свай - сделано, Диаметр пересчитывается по формуле, и вводится Gamma_cc = 1.2 для забивных свай.
надо написать ввод из тестового файла с заданием характеристик по одной колонке.
надо дописать вывод в базу данных.
К пропорциональности вводится в тех еденицах, в которых мы его задаем в Лировской задаче, т.е. в тс/м4.
А также модули упругости грунтов и свай в тс/м2.

Еще вариант как послать на расчет сваи:
start := StartPile new.
dataDict := Dictionary newFrom: { #nodes -> 'nodes.csv' . #l -> 11 . 
#db -> 'kaliningrad2.db' . #column -> 'col861' . #hZagl -> 1 . #r -> (Pile rForKvadr: 0.3). #gcc -> 1.2} . ""Пока не введена возможность указать тип сваи - в будущих версиях""
m := start startPileR: 0.7 dataPilesM: (start dataMain: dataDict). 

""А вот так можно сохранить сериализовать данные""
FLSerializer serialize: m toFileNamed: 'kal1_6piles.fuel'.
""А вот так прочесть обратно""
m_story := FLMaterializer  materializeFromFileNamed: 'kal1_6piles2.fuel'.
"
Class {
	#name : #StartPile,
	#superclass : #SelfCheck,
	#category : #Piles
}

{ #category : #accessing }
StartPile class >> dataColumn_Iges: arrayIges col: colName hZagl: hCol base: base [
	| resBlock col colBlock splitterBlock igeFinder|
	"для автоматического задания колонок введем для каждой колонки заглубление низа фундамента от начала каждой колонки"
	resBlock := [  
	| res con |
	con := UDBCSQLite3Connection new.
	con on: base.
	con open.
	res := con
		execute:
			(''
				join:
					{'select * from '.
					colName}).
	res := res rows.
	con close.
	res ].	
	col := resBlock value. "считали из таблицы col данные"
	col := col collect: [ :row | row data ] .
	igeFinder := [ :arrIges :nameIge |
		|ige|
		arrIges do: [ :each | (each name = nameIge) ifTrue: [ ige := each ] ].
		ige
		  ].
	colBlock := [ col collectWithIndex: [ :line :i | (igeFinder value: arrayIges value: (line at: #ige)) cloneH: (line at: #dh) ] ] . "возвращает список Ige"
	col  := colBlock value.
	(col at: 1) initHsplit: 0.5. "разбивка первого грунта по слоям идет через 0.5м"
	splitterBlock := [ :igesList | |res z| 
		z := 0.
		res := OrderedCollection new.
		igesList do: [ :ige | | z1 z2|  
			z1 := z. z2 := z1 + (ige h) .
			((z2 > hCol) and: (z1 >= hCol)) ifTrue: [ res add: ige ] .
			((z2 > hCol) and: (z1 < hCol)) ifTrue: [ | len | len := z2 - hCol. res add: (ige cloneH: len) ] .
			z := z + (ige h) .
			 ] .		
		res
		 ].
	col := splitterBlock value: col .
	col := IgeColumn initList: col. "преобразуем список грунтов в объект IgeColumn"
	col zFactFictiv.
	^ col
]

{ #category : #accessing }
StartPile class >> dataIgeId: id baseName: base [
	| ige con e mu k res desc |
	con := UDBCSQLite3Connection new.
	con on: base.
	con open.
	res := con
		execute:
			('' join: {'select E, E_suppose, mu, mu_suppose, K, K_suppose, desc, name from [ige'. $'. 's] where id = '.id}).
	res := res rows.
	res := (res at: 1) data.
	e := res at: #E.
	((e isNil) or: (e = ''))
		ifTrue: [ e := res at: #E_suppose ].
	mu := res at: #mu.
	((mu isNil) or: (mu = ''))
		ifTrue: [ mu := res at: #mu_suppose ].
	k := res at: #K.
	((k isNil) or: (k = ''))
		ifTrue: [ k := res at: #K_suppose ].
	desc := res at: #desc.
	name := res at: #name.
	ige := Ige
		createInitName: name 
		desc: desc
		e: e
		mu: mu
		k: k.
	con close.
	^ ige
]

{ #category : #accessing }
StartPile class >> fileOut: liraFileTxt [
	| f |
	f := FileStream newFileNamed: 'piles.txt'.
	f nextPutAll: liraFileTxt.
	f close
]

{ #category : #dataColumns }
StartPile >> dataColumn: dataDict [
	| res base col hZagl |
	hZagl := dataDict at: #hZagl.
	base := dataDict at: #db.
	res := self
		sqliteReadingBase: base
		executeString:
			('select * from [{1}] inner join [ige{2}s] on [{1}].ige = [ige{2}s].name'
				format:
					{(dataDict at: #column).
					$'}).
	res := self dataIges: res.
	res := IgeColumn cutSoleIgeList: res hZagl: hZagl.
	(res at: 1) initHsplit: 0.5.
	col := IgeColumn initList: res.
	col
		zFactFictiv;
		setName: (dataDict at: #column).
	^ col
]

{ #category : #dataIges }
StartPile >> dataIges: arrayIges [
	| valBlock |
	valBlock := [ :st1 :st2 :dict | 
	| v1 |
	v1 := dict at: st1 asSymbol.
	(v1 isNil or: v1 = '')
		ifTrue: [ v1 := dict at: st2 ].
	v1 ].
	^ arrayIges
		collect: [ :each | 
			| e mu k desc name ige|
			e := valBlock value: 'E' value: 'E_suppose' value: each.
			mu := valBlock value: 'mu' value: 'mu_suppose' value: each.
			k := valBlock value: 'K' value: 'K_suppose' value: each.
			desc := each at: #desc.
			name := each at: #name.
			ige := Ige
				createInitName: name
				desc: desc
				e: e
				mu: mu
				k: k .
			ige setH: (each at: #dh).
			ige]
]

{ #category : #dataSetters }
StartPile >> dataMain: dataDict [
	| columnS nodeBlock nodes1 m1 t |
	t := Transcript.
	t
		clear;
		open.
	nodeBlock := [ :nodes | 
	| l m |
	l := FileLira new.
	m := l nodes: nodes.
	m ].
	nodes1 := nodeBlock value: (dataDict at: #nodes).
	columnS := self dataColumn: dataDict.
	m1 := nodes1
		collect: [ :point | 
			(Pile
				calcPileOne_column: columnS
				r: (dataDict at: #r)
				l: (dataDict at: #l)
				e: 3000000
				coord: (point at: 2) asNumber @ (point at: 3) asNumber
				gcc: (dataDict at: #gcc)) clone ].
	^ m1
]

{ #category : #pileEditor }
StartPile >> editPileM: m [
	"метод позволяет выборочно редактировать параметры отдельных свай в массиве свай"

	| xy |
	m do: [ :pile | 
		xy := pile xy.
		xy = (5.6@0.6) ifTrue: []]
]

{ #category : #initialization }
StartPile >> initialize [
	super initialize
]

{ #category : #baseIO }
StartPile >> sqliteReadingBase: base executeString: exString [
	| con res |
	con := SQLite3Connection openOn: base.
	res := con execute: exString.
	res := res rows.
	con close.
	^ res := res
		collect: [ :e | Dictionary newFromKeys: e columnNames andValues: e values ]
]

{ #category : #entry }
StartPile >> startPileR: r dataDict: dataDict [
	| m g stiffM dataPilesM|
	dataPilesM := self dataMain: dataDict .
	dataPilesM do: [ :each | each initAlfaR: r ].
	stiffM := Stiffness new.
	dataPilesM
		do: [ :eachPile | 
			| sad alfa_k tmp8 |
			sad := 0.
			alfa_k := 1.
			dataPilesM
				do: [ :pile2 | 
					(eachPile compareXy: pile2)
						ifTrue: [ sad := sad + (eachPile s_ad: pile2).
							alfa_k := alfa_k * (eachPile alfaEval: pile2) ] ].
			eachPile initSadd: sad.
			stiffM add: eachPile cAll.
			tmp8 := eachPile.
			tmp8
				initAlfaK: alfa_k;
				lrEval ].
	m := Pile xyCallOutM: dataPilesM.
	self
		checkValue: m
		valName: 'm'
		onMethod: 'startPileR: r dataPilesM: dataPilesM'.
	g := Graph2D new.
	g
		initMassiveXYZ: m;
		outSvgWithFilename: 'Жесткости под концами свай.svg'
			title: 'Жесткости под концами свай'.
	m := Pile xyAlfaOutM: dataPilesM.
	g := Graph2D new.
	g
		initMassiveXYZ: m;
		outSvgWithFilename: 'К-ты по боковой поверхности свай.svg'
			title: 'К-ты по боковой поверхности свай'.
	^ dataPilesM
]
