<div align="center">

## PacMan


</div>

### Description

Emulates a pacman game in it's simplest form.
 
### More Info
 


<span>             |<span>
---                |---
**Submitted On**   |
**By**             |[Rudy Boudewijn van Etten](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByAuthor/rudy-boudewijn-van-etten.md)
**Level**          |Unknown
**User Rating**    |5.0 (10 globes from 2 users)
**Compatibility**  |VB 5\.0, VB 6\.0
**Category**       |[Games](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByCategory/games__1-38.md)
**World**          |[Visual Basic](https://github.com/Planet-Source-Code/PSCIndex/blob/master/ByWorld/visual-basic.md)
**Archive File**   |[](https://github.com/Planet-Source-Code/rudy-boudewijn-van-etten-pacman__1-3571/archive/master.zip)





### Source Code

```
' Pacman sourcecode
'
' Ever played pacman? Well here is a sourcecode on making it.
'
' Needs only a timer set on interupt x (any difficullty!)
'
' Paste the code in your new project or HTML project and offer your
' viewers one of the most enjoyable games ever!!
'
' Coded by R.b.v.Etten in 1999
'
'
' Note on graphics!!!
'
' Since I have coded it using only the line command the game lookes a little bit
' boring. If you look at the code more closely (line!) you could change it to bitblt/paint
' and add some real pacman graphics.
Dim lvl(281) 'level data. Plus 1 !!
Dim lvlb(281) 'level data. Plus 1 !!
Dim px As Integer 'positie x
Dim py As Integer 'positie y
Dim ox As Integer 'buffer positie
Dim oy As Integer 'buffer positie
Dim score
Dim levens
'
Dim sx(2) As Integer
Dim sy(2) As Integer
Dim sox(2) As Integer
Dim soy(2) As Integer
Dim sbuf(2) As Integer
Dim dire(2) As Integer
'
Dim lvlv As Integer
'
'
'
Private Sub Form_Load()
ScaleMode = 3 'pixels dus
px = 2: ox = px 'startpositie x
py = 1: oy = py ''startpositie y
'
For i = 0 To 2
sx(i) = 9: sox(i) = sx(i)
sy(i) = 6: soy(i) = sy(i): dire(i) = 4
sbuf(i) = 0
Next
'
'
If lvlv = 0 Then lvlv = 1 Else lvlv = 0
Call leeslvl(lvlv)
score = 0 'zet de score op 0
levens = 3 'zet aantal levens op 3
Call Form_Resize
Timer1.Enabled = True
End Sub
'beweging van pac man via het toetsen bord
Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
ox = px: oy = py 'neem ff de huidige lokatie op
Select Case KeyCode
  Case vbKeyUp: If py > 0 Then py = py - 1: Call doemove
  Case vbKeyDown: If py < 13 Then py = py + 1: Call doemove
  Case vbKeyLeft: If px > 1 Then px = px - 1: Call doemove
  Case vbKeyRight: If px < 20 Then px = px + 1: Call doemove
End Select
End Sub
Private Sub doemove()
posa = ox + (oy * 20) 'kijk op het veld
posb = px + (py * 20) 'kijk op het veld
If lvl(posb) = 1 Then px = ox: py = oy: Exit Sub 'als muurtje dan exit
Call dscore(posb)
lvl(posb) = 4: lvl(posa) = 0: lvlb(posa) = 0 ' nieuwe positie even invoeren en oude uit...
Call Form_Resize
End Sub
Private Sub dscore(pos)
If pos = 0 = False Then
If lvl(pos) = 2 Then score = score + 10 'pilletje 1 +10
If lvl(pos) = 3 Then score = score + 20 ',,,,
End If
'
a = "Simplepacman Score : " + Str(score) 'toon de score in de balk
a = a + "  "
a = a + "Levens : "
a = a + Str(levens) + "  "
If Form1.Caption = a = False Then Form1.Caption = a
End Sub
'
Private Sub spookje(z)
ReDim del(8) As Integer
Dim i As Integer
Dim a As Integer
sox(z) = sx(z): soy(z) = sy(z) ' oude ypos
'
del(0) = lvl((sx(z)) + (sy(z) - 1) * 20)
del(1) = lvl((sx(z) - 1) + sy(z) * 20)
del(2) = lvl((sx(z) + 1) + sy(z) * 20)
del(3) = lvl(sx(z) + (sy(z) + 1) * 20)
'
For i = 0 To 3
If del(i) = 1 = False Then a = a + 1
Next
If a = 3 Then dire(z) = 4
Randomize Timer
If dire(z) = 4 Then
Select Case Fix(Rnd * 5) 'gebaseerd op random beweging
  Case 1
  If del(0) = 1 = False Then dire(z) = 0
  Case 2
  If del(1) = 1 = False Then dire(z) = 1
  Case 3
  If del(2) = 1 = False Then dire(z) = 2
  Case 4
  If del(3) = 1 = False Then dire(z) = 3
  End Select
End If
pop:
'
Select Case dire(z)
Case 0: sy(z) = sy(z) - 1
Case 1: sx(z) = sx(z) - 1
Case 2: sx(z) = sx(z) + 1
Case 3: sy(z) = sy(z) + 1
End Select
'
posa = sox(z) + (soy(z) * 20) 'kijk op het veld
posb = sx(z) + sy(z) * 20 'kijk op het veld
If lvl(posb) = 1 Then sx(z) = sox(z): sy(z) = soy(z): dire(z) = 4: Exit Sub
If lvl(posb) = 4 Then lvl(posb) = 0: Call live 'col detection
If lvl(posa) = 4 Then lvl(posa) = 0: Call live
lvl(posa) = sbuf(z) 'kopieer nieuwe positie in sbuf
sbuf(z) = lvlb(posb) 'kopieer nieuwe positie in sbuf
lvl(posb) = 5   'plaats spookje in nieuwe positie
End Sub
Private Sub live()
levens = levens - 1 ' tel de levens af
px = 3: ox = px    'herstel start positie
py = 1: oy = py    ',,,,,,
If levens = 0 Then Timer1.Enabled = False: Call Form_Load 'levens op dan nieuw spel
Call dscore(0) 'print info in balk
End Sub
' Level draw. Grafisch gedeelte. Blitten kan ook!!
'
Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
n = 1
For ay = 1 To 14
  For ax = 2 To 21
    If lvl(n) = 1 Then k = RGB(0, 0, 0) 'muurtje
    If lvl(n) = 0 Then k = RGB(255, 255, 255) 'open vlak
    If lvl(n) = 2 Then k = RGB(0, 0, 255) 'Pilletje
    If lvl(n) = 3 Then k = RGB(0, 255, 0) 'ander pilletje
    If lvl(n) = 4 Then k = RGB(255, 255, 0) 'Pac man
    If lvl(n) = 5 Then k = RGB(255, 0, 0) 'spookje
    Line (ax * 20, ay * 20)-((ax * 20) + 18, (ay * 20) + 18), k, BF
    n = n + 1
  Next
Next
End Sub
Private Sub leeslvl(n)
'Read level into the array. Edit the a=a+ string to change the level
'experiment and see the effect.
Select Case n
Case 0
    a = "11111111111111111111"
a = a + "13222222222222222231"
a = a + "12121111111111112121"
a = a + "12222222222222222221"
a = a + "12121211111111212121"
a = a + "12121212222221212121"
a = a + "12121212222221212121"
a = a + "12121211122111212121"
a = a + "12121222222222212121"
a = a + "12121211111111212121"
a = a + "12222222222222222221"
a = a + "12121111111111112121"
a = a + "13222222222222222231"
a = a + "11111111111111111111"
Case 1
    a = "11111111111111111111"
a = a + "12222222222222222221"
a = a + "12111111111111111121"
a = a + "12132222222222223121"
a = a + "12121111112111112121"
a = a + "12121222222222212121"
a = a + "12221211111111212221"
a = a + "12121212222221212121"
a = a + "12121222222222212121"
a = a + "12121111112111112121"
a = a + "12132222222222223121"
a = a + "12111111111111111121"
a = a + "13222222222222222221"
a = a + "11111111111111111111"
End Select
For i = 1 To 281 'lees de inhoud van a naar de lvl() dim
  lvl(i) = Mid(a, i, 1)
  lvlb(i) = Mid(a, i, 1)
Next
End Sub
Private Sub Timer1_Timer() ' the timer contains the AI (ghosts,that pose the threat in the game)
For i = 0 To 2: Call spookje(i): Next
Call Form_Resize
End Sub
Private Sub Form_Resize()
Call Form_MouseUp(0, 0, 0, 0) 'hertekenen maar
End Sub
```

