package converge

import com.benasher44.uuid.Uuid
import com.benasher44.uuid.uuid4
import kotlin.jvm.JvmInline
import kotlin.jvm.Synchronized
import kotlin.random.Random

@JvmInline
value class Actor private constructor(val value: Long) {
    companion object {
        fun of(value: Long) = if (value >= 0) {
            Actor(value)
        } else {
            throw IllegalArgumentException("Actor value must be >= 0, but was $value")
        }

        fun lowest() = Actor.of(0)
        fun highest() = Actor.of(Long.MAX_VALUE)

        fun random() =
            Actor.of(Random.nextLong())
    }
}

@JvmInline
value class Counter private constructor(val value: Long) {
    fun next() = Counter.of(value + 1)

    companion object {
        fun of(value: Long) = if (value >= 0) {
            Counter(value)
        } else {
            throw IllegalArgumentException("Counter value must be >= 0, but was $value")
        }

        fun lowest() = Counter.of(0)
        fun highest() = Counter.of(Long.MAX_VALUE)

        fun next(value: Long) = Counter.of(value).next()
    }
}

data class Id(val counter: Counter, val actor: Actor)

sealed class Op {
    data class Root(val id: Uuid, val creator: Actor): Op()
    data class MakeMap(val isRoot: Boolean): Op()
    data class MakeVector(val isRoot: Boolean): Op()
    data class MakeSet(val isRoot: Boolean): Op()
    data class MakeList(val isRoot: Boolean): Op()
    data class MakeText(val isRoot: Boolean): Op()
    data class MakeKey(val value: Any): Op()
    data class MakeValue(val isRoot: Boolean, val value: Any): Op()
    data class Insert(val id: Id): Op()
    data class Assign(
        val entity: Id,
        val attribute: Id,
        val value: Id
    ): Op()
    data class Remove(
        val entity: Id,
        val attribute: Id,
    ): Op()
}

expect interface SortedMap<K, V>: MutableMap<K, V>
expect class TreeMap<K, V>(): SortedMap<K, V> {
    constructor(m: SortedMap<K, V>)
}

class OpSet {
    private val ops: SortedMap<Id, Op>

    constructor(
        creator: Actor = Actor.random(),
        id: Uuid = uuid4(),
    ) {
        ops = TreeMap()
        ops[Id(Counter.lowest(), Actor.lowest())] = Op.Root(id, creator)
    }
    constructor(ops: SortedMap<Id, Op>) {
        this.ops = TreeMap(ops)
        this.creator
    }

    val creator: Actor
        @Synchronized
        get() =
            when(val op = ops.asSequence().first().value) {
                is Op.Root -> op.creator
                else -> throw IllegalStateException("First Op of OpSet isn't a Root!")
            }

    @Synchronized
    operator fun set(id: Id, op: Op) {
        ops[id]?.let {
            throw IllegalArgumentException("Trying to add ($id, $op), but ($id, $it) already present in opset!")
        }
        ops[id] = op
    }

    @Synchronized
    fun interpret(): Interpretation = TODO()
}

data class Element(val e: Id, val a: Id, val v: Id, val t: Id)

sealed interface ListLink {
    data class Link(val to: Id): ListLink
    object End: ListLink
}

typealias EntityLookup = Map<Id, Op>

expect interface SortedSet<E>: MutableSet<E>
expect class TreeSet<E>(): SortedSet<E>

class Interpretation(
    val elements: SortedSet<Element>,
    val listLinks: Map<ListLink, ListLink>,
    val parents: Map<Id, Id>,
    val entities: EntityLookup,
    val keys: EntityLookup,
    val keyCache: Map<Any, Id>,
    val values: EntityLookup,
) {

}