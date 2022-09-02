package converge

interface SortedMap<K, V>: java.util.SortedMap<K, V>
class TreeMap<K, V>: java.util.TreeMap<K, V>, SortedMap<K, V> {
    constructor(): super()
    constructor(comparator: Comparator<K>): super(comparator)
    constructor(m: Map<K, V>): super(m)
    constructor(m: SortedMap<K, V>): super(m)
}

interface SortedSet<T>: java.util.SortedSet<T>
class TreeSet<T>: java.util.TreeSet<T>(), SortedSet<T>