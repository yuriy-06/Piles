"
Класс, реализующий базу данных жесткостей, ввиде OrderedCollection:
есть метод add: -- он добавляет в нее значения, и проверяет на дубли,
если есть -- не  добавляет

    Instance Variables
	Stiff:		база ввиде OrderedCollection

"
Class {
	#name : #Stiffness,
	#superclass : #Object,
	#instVars : [
		'Stiff'
	],
	#category : #Piles
}

{ #category : #adding }
Stiffness >> add: stiffNumber [
	(Stiff includes: stiffNumber)
		ifFalse: [ Stiff add: stiffNumber ].
	^ Stiff
]

{ #category : #initialization }
Stiffness >> initialize [
	super initialize.
	Stiff := OrderedCollection new
]

{ #category : #getters }
Stiffness >> stiffOut [
	^ Stiff
]
