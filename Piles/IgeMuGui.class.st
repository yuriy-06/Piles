"
Класс создан для расчета коэф-та Пуассона грунтов
 пример запуска
m := IgeMuGui start.
"
Class {
	#name : #IgeMuGui,
	#superclass : #Object,
	#instVars : [
		'index1block',
		'index2block',
		'index3block',
		'index4_6block'
	],
	#category : #Piles
}

{ #category : #accessing }
IgeMuGui class >> start [
	| dropListButton n|
	n := IgeMuGui new.
	dropListButton := DropListButton new.
	dropListButton
		displayBlock: [ :item | item asString ];
		items: {'Крупнообломочные грунты' . 'Пески и супеси' . 'Суглинки' . 'Глины: Il <= 0' . 'Глины: 0 < Il <= 0.25' . 'Глины: 0.25 < Il <= 1'};
		label: 'Посчитать';
		extent: 650 @ 70;
		title: 'Вычисление коэффициента поперечной деформации (Пуассона) грунтов'.
	dropListButton button
		action: [ | index |
			index := dropListButton dropList getIndex. "получили индекс выбранного пункта"
			Transcript
				clear;
				open;
				show: 'к-т Пуассона равен: '.
			index = 1
				ifTrue: [ n index1block value ].
			index = 2
				ifTrue: [ n index2block value ].
			index = 3
				ifTrue: [ n index3block value ].
				
			((4 to: 6) includes: index)
				ifTrue: [ n index4_6block value ] ].
	dropListButton openWithSpec.
	^ dropListButton
]

{ #category : #accessing }
IgeMuGui >> index1block [
	^ [ Transcript clear; show: 'к-т Пуассона равен: '; show: '0.27' ]
]

{ #category : #accessing }
IgeMuGui >> index2block [
	^ [ | e val |
	"примерные пределы к-та пористости для песков
					0.55 - 0.3
					0.8 - 0.35
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, коэффициент пористости') asNumber .
	val := (Interpolation int1d_xm: #(0.55 0.8) ym: #(0.3 0.35) x: e) round: 2.
	Transcript clear; show: 'к-т Пуассона равен: '; show: val ]
]

{ #category : #accessing }
IgeMuGui >> index3block [
	^ [ | il val |
	"примерные пределы показателя текучести для суглинков
					0.25 - 0.35 Тугопластичное состояние (но есть, например, твердое и полутвердое)
					0.75 - 0.37 Текучепластичное (Есть текучее)
					"
	il := (UIManager default request:  'Введите, пожалуйста, Сэр, показатель текучести суглинка') asNumber .
	val := (Interpolation int1d_xm: #(0.25 0.75) ym: #(0.35 0.37) x: il) round: 2.
	Transcript clear; show: 'к-т Пуассона равен: ';  show: val ]
]

{ #category : #accessing }
IgeMuGui >> index4_6block [
	^ [ | il val |
	il := (UIManager default request: 'Введите, пожалуйста, Сэр, показатель текучести глины ')
		asNumber.
	il >= 0
		ifTrue: [ Transcript clear; show: 'к-т Пуассона равен: ';  show: '0.3' ].
	il > 0
		ifTrue: [ il <= 0.25
				ifTrue: [ val := (Interpolation int1d_xm: #(0 0.25) ym: #(0.3 0.38) x: il) round: 2.
					Transcript clear; show: 'к-т Пуассона равен: ';  show: val ] ].
	il > 0.25
		ifTrue: [ il <= 1
				ifTrue: [ val := (Interpolation int1d_xm: #(0.25 1) ym: #(0.38 0.45) x: il) round: 2.
					Transcript clear; show: 'к-т Пуассона равен: ';  show: val ] ] ]
]

{ #category : #initialization }
IgeMuGui >> initialize [
	super initialize .
	index1block := self index1block .
	index2block := self index2block .
	index3block := self index3block .
	index4_6block := self index4_6block
	
]
