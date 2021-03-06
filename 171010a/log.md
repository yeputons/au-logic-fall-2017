# Cut
Cut в прологе --- это восклицательный знак в правой части.
Он обрубает перебор в текущей точке.
https://stackoverflow.com/a/15065841/767632.

```prolog
max4bad(X, Y, Z) :- le(X, Y), eq(Y, Z).
max4bad(X, _, X).
```

Тут мы попытались написать: если `le(X, Y)`, то выполняется первое, а иначе второе.
Но на самом деле второе выполняется всегда.
Чтобы добавить это самое "иначе", надо добавить `!`:

```prolog
max4(X, Y, Z) :- le(X, Y), !, eq(Y, Z).
max4(X, _, X).
```

Говорим, что если `le(X, Y)` выполнилось, то в другие ветки ходить никогда не надо.
Таким образом, `!` просто обрубает backtracking и ветки перебора между выводом конкретного `max4` и `!`.
Обращаем внимание, что он обрубает не вообще весь backtracking, а только некий хвост.
Видимо, до последнего решения, которое было принято не при выводе текущего правила (см `02-cut.pl`).

## Семантика negations failure
```prolog
+/(A) :- A,!,fail.
+/(A).
```

Получили этакое отрицание. Если `A` верно, то оно докажется и из-за cut мы сфейлимся.
А иначе докажется `+/(A)`, получили "типа отрицание".

Но есть проблема со свободными переменными.
```prolog
p(X) :- eq(X, 123).
```
Если спросить `p(_)`, то будет true, потому что есть переменная: `\exists x: p(x)`.
Но если спросить `+/(p(_))`, то будет `false`, несмотря на то, что есть переменная, при которой это верно.
Получаем, что на самом деле у нас отрицается вместе с квантором: `\not(\exists x: p(x)) <=> \forall x: \not p(x)`.
Итого: нелогичность.

## Predicate completion
Есть интерпретация "predicate completion", в которой отрицание всё-таки разумно:
1. Заменяем все `:-` на `<=>`
2. Если факт не встречается в левых частях, то добавляем его отрицание.

TODO: въехать
