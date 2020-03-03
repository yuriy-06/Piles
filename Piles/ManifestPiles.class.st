"
Пакет для программы генерации комп. модели Лиры свайного пола

послать на расчет сваи:
start := StartPile new.
dataDict := Dictionary newFrom: { #nodes -> 'nodes.csv' . #l -> 11 . 
#db -> 'kaliningrad2.db' . #column -> 'col861' . #hZagl -> 1 . #r -> (Pile rForKvadr: 0.3). #gcc -> 1.2} . ""Пока не введена возможность указать тип сваи - в будущих версиях""
m := start startPileR: 0.7 dataPilesM: (start dataMain: dataDict). 

""А вот так можно сохранить сериализовать данные""
FLSerializer serialize: m toFileNamed: 'kal1_6piles.fuel'.
""А вот так прочесть обратно""
m_story := FLMaterializer  materializeFromFileNamed: 'kal1_6piles2.fuel'.

Но эти данные еще надо вывести в Лиру:
l := LiraFile new.
ll := l startM: m.

f := FileStream newFileNamed: 'piles.txt'.
f nextPutAll: ll.
f close.
"
Class {
	#name : #ManifestPiles,
	#superclass : #PackageManifest,
	#category : #'Piles-Manifest'
}
