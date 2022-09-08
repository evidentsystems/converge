package converge

actual interface SortedMap<K, V> : MutableMap<K, V> {
    fun comparator(): Comparator<in K>?
    fun subMap(k: K, k1: K): SortedMap<K, V>?
    fun headMap(k: K): SortedMap<K, V>?
    fun tailMap(k: K): SortedMap<K, V>?
    fun firstKey(): K
    fun lastKey(): K
    fun keySet(): Set<K>
    fun entrySet(): Set<Map.Entry<K, V>>
}

actual class TreeMap<K, V> : SortedMap<K, V> {
    actual constructor()
    actual constructor(m: SortedMap<K, V>)

    override val entries: MutableSet<MutableMap.MutableEntry<K, V>>
        get() = TODO("Not yet implemented")
    override val keys: MutableSet<K>
        get() = TODO("Not yet implemented")
    override val size: Int
        get() = TODO("Not yet implemented")
    override val values: MutableCollection<V>
        get() = TODO("Not yet implemented")

    override fun comparator(): Comparator<in K>? {
        TODO("Not yet implemented")
    }

    override fun firstKey(): K {
        TODO("Not yet implemented")
    }

    override fun lastKey(): K {
        TODO("Not yet implemented")
    }

    override fun keySet(): Set<K> {
        TODO("Not yet implemented")
    }

    override fun entrySet(): Set<Map.Entry<K, V>> {
        TODO("Not yet implemented")
    }

    override fun tailMap(k: K): SortedMap<K, V>? {
        TODO("Not yet implemented")
    }

    override fun headMap(k: K): SortedMap<K, V>? {
        TODO("Not yet implemented")
    }

    override fun subMap(k: K, k1: K): SortedMap<K, V>? {
        TODO("Not yet implemented")
    }

    override fun clear() {
        TODO("Not yet implemented")
    }

    override fun isEmpty(): Boolean {
        TODO("Not yet implemented")
    }

    override fun remove(key: K): V? {
        TODO("Not yet implemented")
    }

    override fun putAll(from: Map<out K, V>) {
        TODO("Not yet implemented")
    }

    override fun put(key: K, value: V): V? {
        TODO("Not yet implemented")
    }

    override fun get(key: K): V? {
        TODO("Not yet implemented")
    }

    override fun containsValue(value: V): Boolean {
        TODO("Not yet implemented")
    }

    override fun containsKey(key: K): Boolean {
        TODO("Not yet implemented")
    }
}

interface SortedSet<E> : MutableSet<E> {
    fun comparator(): Comparator<in E?>?

    fun subSet(e: E?, e1: E?): SortedSet<E?>?

    fun headSet(e: E?): SortedSet<E?>?

    fun tailSet(e: E?): SortedSet<E?>?

    fun first(): E?

    fun last(): E?
}

class TreeSet<T> : SortedSet<T> {
    override val size: Int
        get() = TODO("Not yet implemented")

    override fun comparator(): Comparator<in T?>? {
        TODO("Not yet implemented")
    }

    override fun first(): T? {
        TODO("Not yet implemented")
    }

    override fun last(): T? {
        TODO("Not yet implemented")
    }

    override fun tailSet(e: T?): SortedSet<T?>? {
        TODO("Not yet implemented")
    }

    override fun headSet(e: T?): SortedSet<T?>? {
        TODO("Not yet implemented")
    }

    override fun subSet(e: T?, e1: T?): SortedSet<T?>? {
        TODO("Not yet implemented")
    }

    override fun add(element: T): Boolean {
        TODO("Not yet implemented")
    }

    override fun addAll(elements: Collection<T>): Boolean {
        TODO("Not yet implemented")
    }

    override fun clear() {
        TODO("Not yet implemented")
    }

    override fun isEmpty(): Boolean {
        TODO("Not yet implemented")
    }

    override fun containsAll(elements: Collection<T>): Boolean {
        TODO("Not yet implemented")
    }

    override fun contains(element: T): Boolean {
        TODO("Not yet implemented")
    }

    override fun iterator(): MutableIterator<T> {
        TODO("Not yet implemented")
    }

    override fun retainAll(elements: Collection<T>): Boolean {
        TODO("Not yet implemented")
    }

    override fun removeAll(elements: Collection<T>): Boolean {
        TODO("Not yet implemented")
    }

    override fun remove(element: T): Boolean {
        TODO("Not yet implemented")
    }
}