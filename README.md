# Basic migrations library for Hasql

![image](https://github.com/user-attachments/assets/26eaf5a5-0940-42e9-9be4-236b400b6565)

## Features

- up/down
- Divergent (changed) migration detection - i.e., can use it in a development environment fairly easily
- Support for force-up/force-down of a chosen migration

## Usage

```haskell
-- GHC extensions: Probably all of them.
module Migrations where
import Hasql.Mover

main :: IO ()
main = hasqlMover @'[V0_Init, V1_AddSomething]

[declareMigration|
name = V0_Init

[up]
CREATE TABLE foobar();

[down]
DROP TABLE foobar CASCADE;
|]

[declareMigration|
name = V1_AddSomething

[up]
CREATE TABLE foobarbaz();

[down]
DROP TABLE foobarbaz CASCADE;
|]
```
