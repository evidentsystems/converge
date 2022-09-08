package converge

import com.benasher44.uuid.Uuid
import com.benasher44.uuid.uuid4
import kotlin.jvm.JvmInline
import kotlin.jvm.Synchronized
import kotlin.random.Random

@JvmInline
value class Actor private constructor(val value: Long) {
    companion object {
        val MIN = of(0)
        val MAX = of(Long.MAX_VALUE)

        fun of(value: Long) = if (value >= 0) {
            Actor(value)
        } else {
            throw IllegalArgumentException("Actor value must be >= 0, but was $value")
        }

        fun random() =
            of(Random.nextLong())
    }
}

@JvmInline
value class Counter private constructor(val value: Long) {
    fun next() = of(value + 1)

    companion object {
        val MIN = of(0)
        val MAX = of(Long.MAX_VALUE)

        fun of(value: Long) = if (value >= 0) {
            Counter(value)
        } else {
            throw IllegalArgumentException("Counter value must be >= 0, but was $value")
        }

        fun next(value: Long) = of(value).next()
    }
}

data class Id(val counter: Counter, val actor: Actor) {
    companion object {
        val MIN = Id(Counter.MIN, Actor.MIN)
        val MAX = Id(Counter.MAX, Actor.MAX)
    }
}

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

private fun isAncestor(
    parents: Map<Id, Id>,
    entity: Id,
    possibleAncestor: Id
): Boolean {
    var i = entity
    var parent = parents[i]
    while(parent != null) {
        if (parent == possibleAncestor) {
            return true
        } else {
            i = parent
            parent = parents[i]
        }
    }
    return false
}

class OpSet {
    private val ops: SortedMap<Id, Op>

    constructor(
        creator: Actor = Actor.random(),
        id: Uuid = uuid4(),
    ) {
        ops = TreeMap()
        this[Id.MIN] = Op.Root(id, creator)
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
    fun interpret(startingInterpretation: Interpretation?): Interpretation {
        val elements = TreeSet<Element>()
        val listLinks = mutableMapOf<ListLink, ListLink>()
        val parents = mutableMapOf<Id, Id>()
        val entities = mutableMapOf<Id, Op>()
        val keys = mutableMapOf<Id, Op>()
        val keyCache = mutableMapOf<Any, Id>()
        val values = mutableMapOf<Id, Op>()
        for ((id, op) in ops) {
            when(op) {
                is Op.Root -> {}
                is Op.MakeMap, is Op.MakeSet, is Op.MakeText -> {
                    entities[id] = op
                }
                is Op.MakeList, is Op.MakeVector -> {
                    entities[id] = op
                    listLinks[ListLink.Link(id)] = ListLink.End
                }
                is Op.MakeKey -> {
                    keys[id] = op
                    keyCache[op.value] = id
                }
                is Op.MakeValue -> {
                    values[id] = op
                }
                is Op.Insert -> {
                    val prev = ListLink.Link(op.id)
                    listLinks.remove(prev)?.let { next ->
                        listLinks[prev] = ListLink.Link(id)
                        listLinks[ListLink.Link(id)] = next
                    }
                }
                is Op.Assign -> {
                    if (!isAncestor(parents, op.entity, op.value)) {
                        // Add new element and parent relationship
                        parents[op.value] = op.entity
                        elements.add(Element(op.entity, op.attribute, op.value, id))
                        // Remove elements with matching entity and attribute (from subrange of entity+attribute)
                        TODO("Remove elements with matching entity and attribute (from subrange of entity+attribute)")
                        // If value is already in the entities map, remove all elements having this value (from subrange of entity)
                        TODO("If value is already in the entities map, remove all elements having this value (from subrange of entity)")
                    }
                }
                is Op.Remove -> {
                    // Remove elements and (parents of the element's value?) with matching entity and attribute (from subrange of entity+attribute)
                    TODO("Remove elements and (parents of the element's value?) with matching entity and attribute (from subrange of entity+attribute)")
                }
            }
        }
        return Interpretation(
            elements, listLinks, parents,
            entities, keys, keyCache, values
        )
    }
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
)