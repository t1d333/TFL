## Запуск алгоритма Angluin

В директории проекта необходимо выполнить 

```
$ cargo run <SYMBOLS> <ORACLE> <MAX_LEN> <TRY_COUNT>
```

 Например 

```
$ cargo run ab ./oracle.sh 20 100
```

- SYMBOLS -- алфавит
- ORACLE -- путь к программе с оракулом
- MAX_LEN -- максимальная длина слова для проверки на эквивалентность языков
- TRY_COUNT -- количество проверок для проверки на эквивалентность

Оракул должен выводить в stdout 1, если слово принадлежит языку, 0 иначе.