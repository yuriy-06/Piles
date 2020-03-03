"
здесь будет разная математика для свай

обращаю внимание на то, что для Numbers реализован 
self circleSquareOfR 
"
Class {
	#name : #PileMath,
	#superclass : #Object,
	#category : #Piles
}

{ #category : #'math functions' }
PileMath class >> squareCircleOfR: r [
	"вычисляется площадь сваи"

	^ 3.142 * r squared
]
