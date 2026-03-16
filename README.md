Repozytorium zawiera rozszerzoną wersję kompilatora Latte.
Do kompilacji można użyć dostarczonego pliku Makefile.
Kompilator wymaga C++17.
Katalog src/ zawiera wymagane pliki źródłowe kompilatora.
Biblioteka runtime znajduje się osobno w katalogu runtime/ i jest linkowana podczas tworzenia plików wykonywalnych.
Projekt nie wykorzystuje żadnych zewnętrznych bibliotek niebędących biblioteką standardową C++.

Zaimplementowane przeze mnie zmiany względem wersji podstawowej można podzielić na dwie kategorie:
1. Poprawki podstawowej wersji kompilatora
   - poprawiłem generowany kod assemblera tak, by wydajniej korzystał z rejestrów. Rozwiązanie oparłem o pomysł przedstawiony w moim mailu, czyli hybrydowy stos, którego wierzchnie elementy są trzymane w rejestrach. Zdecydowałem się na nie wykorzystywanie puli rejestrów callee-saved ze względu na duży overhead wywołań funkcji (wiele pushów i popów w prologu i epilogu) oraz stopień wykorzystania przy wyliczaniu wyrażeń (wartości w dostępnych rejestrach rzadko muszą być spillowane do prawdziwego stosu).
   - zaimplementowałem generację kodu skaczącego (branching dla warunków)
   - poprawiłem generację kodu dla while, by lepiej korzystał z przelatywania między blokami
   - poprawiłem wczytywanie przez readString, aby nie zawierało znaku końca linii

2. Optymalizacje
   - zaimplementowałem optymalizację wyrażeń końcowych opartą o recykling ramek funkcji. Podczas działania frontendu wyliczam rozmiar ramki konieczny dla bezpiecznego przekazania argumentów wywołania ogonowego, a podczas wywołania funkcji przekazuję na stosie odpowiednio duży padding. Warto zauważyć, że nie zawsze jest to możliwe dla funkcji main, ponieważ wygenerowany przez kompilator kod nie jest odpowiedzialny za jej wywołanie. Ze względu na to zdecydowałem się na optymalizację jedynie tych wywołań ogonowych z main, które nie będą wymagały argumentów przekazywanych przez stos.

Zamieszczam również proste testy pokazujące działanie stosu hybrydowego oraz optymalizacji wywołań ogonowych. Dla stosu hybrydowego podaję również odpowiadające mu skrypty .py, dla porównania wyników.
