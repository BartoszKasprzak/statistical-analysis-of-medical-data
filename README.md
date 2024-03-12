1. FORMAT DANYCH
Dane wprowadzane do pliku powinny być zapisane w pliku CSV, gdzie pierwszą kolumną
powinna być badana grupa (np. CHOR1, CHOR2, KONTROLA). Separatorem w pliku
powinien być “;”. Przykład poprawnie zapisanych danych:
grupa;plec;wiek;hsCRP;ERY;PLT;HGB;HCT;MCHC;MON;LEU
CHOR1;k;36;2,711;4,19;201;13,21;0,392;34,7149;0,48;11,86

2. URUCHAMIANIE PROGRAMU I ZAPIS DANYCH
   
Uruchomienie programu

Program napisany został w trybie wsadowym, uruchomić należy go więc z wiersza poleceń
CMD. Aby uruchomić program należy użyć komendy:
“[ścieżka do programu R]” CMD BATCH --vanilla “--args [ścieżka do pliku wejściowego
.CSV z zmienionymi ‘\’ na ‘\\’]” “[ścieżka do skryptu .R]”
Przykładowe użycie komendy:
"C:\Program Files\R\R-4.2.2\bin\R.exe" CMD BATCH --vanilla "--args
C:\\Users\\bkasp\\Downloads\\przykladoweDane-Projekt1.csv"
"C:\Users\bkasp\Desktop\Programowanie\Bartosz-Kasprzak-151545-Projekt.R"

Zapis do pliku

Raport z przeprowadzonych analiz statystycznych, a także zapis wszystkich stworzonych
wykresów zapisany zostaje automatycznie przez program do plików:
raport: ProjektBartoszKasprzak.txt
wykresy: ProjektBKwykresy.pdf
Pliki te znaleźć można w folderze w którym wywoływany jest program, to znaczy w folderze
“zapisanym przed ‘>’” w CMD.

3. DZIAŁANIE PROGRAMU
Program podzielić można kilka fragmentów:

I. Pobieranie danych
Przy pomocy funkcji read.csv2, program zapisuje dane z pliku .CSV do data.frame.
Następnie wszystkie znalezione w danych NA (brak danych) zastępowane są medianą z
całej kolumny. Uzupełnione dane, przy pomocy funkcji split(), dzielone są na
podstawie grup, a następnie zapisywane są do listy z ilością podlist odpowiadającą
ilości grup w pliku z danymi (każda podlista to jedna grupa posiadająca wszystkie
badane elementy).

II. Wstępna charakterystyka danych

a) Wartości odstające
Wartości odstające zostają stworzone przy pomocy funkcji boxplot.stats().
Funkcja ta tworzy listę dla podanych danych, gdzie zapisuje informację między
innymi o wartościach odstających jako obiekt “out” oraz ilość elementów w
podanych danych.

b) Charakterystyka danych
Charakterystyka zostaje stworzona dla wszystkich elementów poza grupą. W
zależności od rodzaju danych, używane są inne funkcję do scharakteryzowania
danych. W przypadku danych nienumerycznych używana jest funkcja table(),
która tworzy statystykę ile powtórzeń danego obiektu znajduje się w danej
grupie, np.: ilość kobiet i mężczyzn w grupie.
W przypadku danych numerycznych używana zostaje funkcja summary(), która
zwraca informacje o: minimalnej wartości badania w grupie, 1 kwartylu badania
w grupie, medianę wartości w badaniu w grupie, średnią arytmetyczną wartości
w badaniu w grupie, trzeci kwartyl badania w grupie oraz maksymalną wartość
badania w grupie. Do funkcji summary dodane zostają jeszcze funkcje: sd() -
zwracająca standardowe odchylenie oraz var() - zwracająca wariancję badania
w grupie.

c) Sprawdzenie rozkładu normalnego
Aby sprawdzić rozkład normalny zostaje użyta funkcja shapiro.test(). Z funkcji
tej wyciągnięte zostaje p.value, dzięki któremu określić można czy dane
posiadają rozkład normalny.
Jeżeli p.value > 0.05 - dane posiadają rozkład normalny, w innym wypadku
dane nie posiadają rozkładu normalnego.

d) Sprawdzenie homogeniczności wariancji
Aby sprawdzić homogeniczność wariancji zostaje użyta funkcja leveneTest(). Z
wyniku tej funkcji wyciągnięte zostaje p.value, dzięki któremu określić można
czy dane posiadają wariancję homogeniczną.
Jeżeli p.value > 0.05 - dane posiadają rozkład normalny, w innym wypadku
dane nie posiadają rozkładu normalnego.

III. Przeprowadzenie testów pomiędzy grupami dla danego badania

W zależności od ilości grup oraz homogeniczności wariancji i rozkładu normalnego
przeprowadzone zostają różne testy na danych, jednak z każdego z nich wyciągnięte
zostaje p.value, które określa czy zachodzą istotne statystycznie różnice pomiędzy
grupami, czy też nie. Jeżeli p.value < 0.05 dla badania zachodzą istotne statystycznie
różnice pomiędzy grupami. W przypadku testu Kruskala-Wallisa oraz ANOVA
wywołane zostają dodatkowe funkcje, które określają pomiędzy którymi grupami
występują grupy. Są to w kolejności: dunnTest oraz TukeyHSD
Przeprowadzane testy:

a) Test t-Studenta
Ilość grup: 2
Rozkład normalny: TAK
Homogeniczność funkcji: 1. TAK; 2. NIE
Użyta funkcja: 1. t.test(), gdzie var.equal = FALSE; 2. t.test(), gdzie var.equal
= TRUE

b) Test Wilcoxonna
Ilość grup: 2
Rozkład normalny: NIE
Homogeniczność funkcji: -
Użyta funkcja: wilcox.test()

c) Test ANOVA
Ilość grup: > 2
Rozkład normalny: TAK
Homogeniczność funkcji: TAK
Użyta funkcja: summary(aov())

d) Test Kruskala - Wallisa
Ilość grup: > 2
Rozkład normalny: TAK / NIE
Homogeniczność funkcji: NIE / -
Użyta funkcja: kruskal.test()

IV. Sprawdzenie korelacji

Korelacja pomiędzy badaniami w grupie sprawdzana jest przy pomocy funkcji
cor.test(), z użyciem metody “spearman”. Aby dowiedzieć się czy i jaka korelacja
zachodzi pomiędzy badaniami należy z wyniku wyciągnąć wartość estimate oraz
p.value. Jeżeli p.value >= 0.05 nie ma korelacji pomiędzy badaniami, w przeciwnym
wypadku należy sprawdzić wartość estimate. W zależności od niej określić można
korelację w następujący sposób:
−1 < r ≤ −0.7 bardzo silna korelacja ujemna
−0.7 < r ≤ −0.5 silna korelacja ujemna
−0.5 < r ≤ −0.3 korelacja ujemna o średnim natężeniu
−0.3 < r ≤ −0.2 słaba korelacja ujemna
−0.2 < r < 0.2 brak korelacji
0.2 ≤ r < 0.3 słaba korelacja dodatnia
0.3 ≤ r < 0.5 korelacja dodatnia o średnim natężeniu
0.5 ≤ r < 0.7 silna korelacja dodatnia
0.7 ≤ r < 1 bardzo silna korelacja dodatnia

V. Zapis do plików
Zapis wykresów tworzonych przez:
wartości odstające - boxplot()
rozkład normalny- plot(density())
curve(dnorm())
Zapis wykresów zachodzi w sposób automatyczny do pliku .pdf otwartego na początku
działania programu przy użyciu funkcji pdf() oraz zamkniętym na końcu dev.off().
Zapis do raportu odbywa się poprzez wpisanie każdego tekstu oraz wyniku pojedynczo
przy pomocji funkcji write().
