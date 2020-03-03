"
класс, который скажет нам, находится ли граница в пределах слоя
"
Class {
	#name : #IntervalH,
	#superclass : #Object,
	#instVars : [
		'Z1',
		'Z2'
	],
	#category : #Piles
}

{ #category : #testing }
IntervalH >> includesIge: ig [
	ig z1 >= Z1
		ifTrue: [ ig z2 <= Z2
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #otherSetters }
IntervalH >> initZ1: z1 z2: z2 [
	Z1 := z1.
	Z2 := z2
]

{ #category : #getters }
IntervalH >> z1 [
	^ Z1
]

{ #category : #getters }
IntervalH >> z2 [
	^ Z2
]
