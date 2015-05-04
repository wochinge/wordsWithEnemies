# **Words with Enemies**
***
#####Studienarbeit im Rahmen des Faches Funktionale Programmierung im SS15 bei Prof. Dr. Braun

## How to use

* Um den Server zu starten:  
 `cabal run -- -p <port>`

## Lösungsmöglichkeiten bei Fehlern

* *Unknown symbols* oder Problem mit *x86_x64 architecture*:  
    * Alle .hs aus library auch in exposed modules aufführen
* `cabal install` funktioniert nicht und Fehlermeldung alà *backjump limit reached*
    * `cabal install --max-backjumps=-1`
    * https://github.com/snapframework/snap-core/issues/195