# maps

Finite map types with a common API

**Package:** net.bardcode.folio.maps<br>
**Exports:** :alist :alist?
   :contains-key? :contains-value?
   :get-key
   :keys
   :map :map? :merge
   :plist :plist? :put-key
   :values
   :wb-map :wb-map?

The **maps** package provides implementations of functional finite maps and a uniform API for working with them. Implementations of the **maps** functions are provided for **alists**, **plists**, and FSet's **wb-map** representation of finite maps. Adding additional representations of maps is a simple matter of specializing the generic functions in the **Maps** package.

An obvious question is why the **Maps** functions are not specialized on Common Lisp's **hash-table** type; surely a hash-table is a kind of finite map? 

The reason that folio doesn't specialize the **Maps** functions for **hash-table** is that the **Maps** protocol is a functional protocol; it assumes the maps it operates on are immutable. The whole advantage of a hash-table is that it provides efficient lookup and update through destructive modification of the table. It's certainly *possible* to implement a purely-functional API for Common Lisp's hash-tables, but it would be perverse to do so, since it would squander all of the performance advantages of hash-tables.


## Reference

<br>
**alist**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A Lisp list whose elements are pairs, represented as **cons** cells. The left element of each pair is a key; the right element is a value associated with the key.

<br>
**alist?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is an **alist**, and false otherwise.

<br>
**contains-key?** *map* *key* &key (test 'eql) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *map* contains *key*, and false otherwise. The function passed in the **test** parameter is used to test whether *key* matches a key in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**contains-value?** *map* *value* &key (test 'eql)  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *map* contains *value*, and false otherwise. The function passed in the **test** parameter is used to test whether *value* matches a value in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**get-key** *map* *key* &key (test 'eql)(default nil) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns the value associated in *map* with *key*, or, if *key* is not present in *map*, returns *default*.

**test** is ignored when *map* is a **wb-map**.

<br>
**keys** *map* => *keys*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a sequence of *keys* that appear in *map*.

<br>
**make** 'map &key (contents nil) => *map* &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **map** instance. The keys and values of the new **map** are given as a **plist** passed in **contents**. The representation of the **map** is chosen by folio, but you can pass it to **as** to obtain a particular type of **map**.

<br>
**map?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true is *thing* is a **map**, and false otherwise.

<br>
**merge** *map1* *map2* =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **map** of the same type as *map1*. The new **map** 

<br>
**plist**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

<br>
**plist?**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

<br>
**put-key**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

<br>
**values**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

<br>
**wb-map**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

<br>
**wb-map?**  =>   &nbsp;&nbsp;&nbsp;&nbsp;[**]<br>
Returns 

