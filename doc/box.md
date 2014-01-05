# box
A mutable container for values.

**Package:** net.bardcode.folio.boxes<br>
**Exports:** box box? set-box! unbox

The **box** type is a mutable container for arbitrary values. The functions **unbox** and **set-box!** can be used to retrieve and replace the value stored in a **box**.

Boxes provide a way to introduce mutability piecemeal into immutable and pure-functional data structures. As an example, we can use a pure-functional implementation of finite maps, but store the value elements in boxes so that they can be destructively updated. All operations on the finite map as a whole and on its keys remain purely functional, but we can destructively modify the values stored on the keys.

A type definition establishes the **box** type as a synonym for a **cons** cell whose **car** element is the keyword **:BOX**.

## Reference

**as** 'box *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new box that contains *value*.

**box**  &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A **box** is a mutable container for a value. folio represents a **box** as a
**cons** whose **car** is the keyword **:box**.

**box** *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **box** containing *value*.

**box?** *value* => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a true value if *value* is a **box**, and a false value otherwise.

**make** 'box &key (*value* nil) => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **box** whose initial value is *value*.

**set-box!** *box* *value* => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Replaces the value of *box* with *value*, returning *value*.

**unbox** box => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns the value stored in *box*.

**setf** (unbox *box*) *value* => *value*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Replaces the value of *box* with *value*, returning *value*.





