"
Класс создан для расчета коэф-та K грунтов
 пример запуска
m := Kcalc start.
"
Class {
	#name : #Kcalc,
	#superclass : #IgeMuGui,
	#instVars : [
		'blockArray'
	],
	#category : #Piles
}

{ #category : #accessing }
Kcalc class >> start [
	| dropListButton n|
	n := Kcalc new.
	dropListButton := DropListButton new.
	dropListButton
		displayBlock: [ :item | item asString ];
		items: {'Пески крупные (0,55 <=e<=0.7);'
		.' глины и суглинки твердые (Il<0)' . 
		'Пески мелкие (0.6<=e<=0.75);'.
		' пески средней крупности (0.55<=e<=0.7)>,'.
		' супеси твердые (Il<0);'.
		' глины и суглинки тугопластичные и полутвердые (0<=Il<=0.75)' . 
		'Пески пылеватые (0.6<=e<=0.8);'.
		' супеси пластичные (0<=Il<=0.75);'.
		' глины и суглинки мягкопластичные (0.5<=Il<=0.75)' . 
		'Глины и суглинки текучепластичные (0.75<=Il<=1)' . 
		'Пески гравелистые (0.55<=e<=0.7);'.
		' крупнообломочные грунты с песчаным заполнителем' };
		label: 'Посчитать';
		extent: 650 @ 70;
		title: 'Определение коэф-та пропорциональности К грунтов (тс/м4)'.
	dropListButton button
		action: [ | index |
			index := dropListButton dropList getIndex. "получили индекс выбранного пункта"
			Transcript
				clear;
				open;
				show: 'К равен: '.
			(n blockArray at: index) value].
	dropListButton openWithSpec.
	^ dropListButton
]

{ #category : #accessing }
Kcalc >> blockArray [
	^ blockArray
]

{ #category : #accessing }
Kcalc >> index10block [
	^ [ | e val |
					"Глины и суглинки текучепластичные (0,75 ≤I L ≤ 1)
					0.75 - 1
					235 - 135
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, показатель текучести') asNumber .
	val := (Interpolation int1d_xm: #(0.5 0.75) ym: #(235 135) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index11block [
	^ [ | e val |
					"Пески гравелистые (0,55 ≤е≤ 0,7)
					0.55 - 0,7
					3335 - 1675
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, к-т пористости') asNumber .
	val := (Interpolation int1d_xm: #(0.55 0.7) ym: #(3335 1675) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index12block [
	"крупнообломочные грунты с песчаным заполнителем"

	^ [ Transcript
		clear;
		show: 'K равен: ';
		show: '1675 тс/м4' ]
]

{ #category : #accessing }
Kcalc >> index1block [
	^ [ | e val |
	"Пески крупные
					0.7 - 0.55
					600 - 1000
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, коэффициент пористости') asNumber .
	val := (Interpolation int1d_xm: #(0.55 0.7) ym: #(1000 600) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index2block [
	"глины и суглинки твердые
(I L < 0)"

	^ [ Transcript
		clear;
		show: 'K равен: ';
		show: '600 тс/м4' ]
]

{ #category : #accessing }
Kcalc >> index3block [
	^ [ | e val |
	"Пески мелкие (0,6 ≤ е ≤ 0,75)
					0.75 - 0.6
					400 - 600
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, коэффициент пористости') asNumber.
	val := (Interpolation int1d_xm: #(0.6 0.75) ym: #(600 400) x: e) round: 1.
	Transcript
		clear;
		show: 'K равен: ';
		show: val;
		show: ' тс/м4' ]
]

{ #category : #accessing }
Kcalc >> index4block [
	^ [ | e val |
	"пески средней крупности (0,55  0,7)
					0.7 - 0.55
					400 - 600
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, коэффициент пористости') asNumber.
	val := (Interpolation int1d_xm: #(0.55 0.7) ym: #(600 400) x: e) round: 1.
	Transcript
		clear;
		show: 'K равен: ';
		show: val;
		show: ' тс/м4' ]
]

{ #category : #accessing }
Kcalc >> index5block [
	"супеси твердые (I L < 0)"

	^ [ Transcript
		clear;
		show: 'K равен: ';
		show: '400 тс/м4' ]
]

{ #category : #accessing }
Kcalc >> index6block [
	^ [ | e val |
					"глины и суглинки тугопластичные и полутвердые (0 ≤ I L ≤ 0,75)
					0.0 - 0.75
					600 - 400
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, показатель текучести') asNumber .
	val := (Interpolation int1d_xm: #(0.0 0.75) ym: #(600 400) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index7block [
	^ [ | e val |
					"Пески пылеватые (0,6 ≤ е ≤ 0,8)
					0.6 - 0.8
					400 - 235
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, коэффициент пористости') asNumber .
	val := (Interpolation int1d_xm: #(0.6 0.8) ym: #(400 235) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index8block [
	^ [ | e val |
					"супеси пластичные (0 ≤ I L ≤0,75)
					0.6 - 0.8
					400 - 235
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, показатель текучести') asNumber .
	val := (Interpolation int1d_xm: #(0.6 0.8) ym: #(400 235) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> index9block [
	^ [ | e val |
					"глины и суглинки мягкопластичные (0,5 ≤ I L ≤ 0,75)
					0.5 - 0.75
					400 - 235
					"
	e := (UIManager default request: 'Введите, пожалуйста, Сэр, показатель текучести') asNumber .
	val := (Interpolation int1d_xm: #(0.5 0.75) ym: #(400 235) x: e) round: 1.
	Transcript clear; show: 'K равен: '; show: val ; show: ' тс/м4']
]

{ #category : #accessing }
Kcalc >> initialize [
blockArray := {self index1block . self index2block . self index3block . self index4block . self index5block . self index6block . self index7block . self index8block . self index9block . self index10block . self index11block . self index12block }
]
