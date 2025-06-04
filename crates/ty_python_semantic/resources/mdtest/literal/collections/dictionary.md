# Dictionaries

## Empty dictionary

```py
reveal_type({})  # revealed: dict[Unknown, Unknown]
```

## Basic dictionary

```py
reveal_type({"a": 1})  # revealed: dict[str, int]
```

## Heterogeneous dictionary

```py
reveal_type({"a": 1, "b": "c"})  # revealed: dict[str, int | str]
```

## Destructuring dictionaries

```py
a = {"a": 1}
reveal_type({**a})  # revealed: dict[str, int]
reveal_type({1: 2, **a})  # revealed: dict[int | str, int]
```
