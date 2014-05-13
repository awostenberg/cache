module Cache


/// provide an abstract interface the differend kinds of caches can subclass
[<AbstractClass>]
type ICache<'k,'v when 'k : equality>() = 
    abstract member TryGetValue: 'k -> 'v option 
    abstract member Add: 'k -> 'v -> 'v
    member this.memoize fn =
      let cache = this
      (fun x ->
        match cache.TryGetValue x with
        | Some v -> v
        | None -> cache.Add x (fn x))

module Naive =
  /// A naive 1-level cache that knows no bounds.
  /// Offered more for a warmup and tutorial exercise to iteratively exlpore the design space than customer-facing
  type UBCache<'k,'v when 'k : equality>() =
      inherit ICache<'k,'v>() 
      let cache = new System.Collections.Generic.Dictionary<'k,'v>()
      override this.TryGetValue k = 
        match cache.TryGetValue k with
        | true,v -> Some v
        | false,_ -> None
      override this.Add k v = cache.Add(k,v);v

  /// A slightly less naive 1-level cache that knows bounds and has a hardwired lru policy
  type N1Cache<'k,'v when 'k : equality>(nslots) =
      inherit ICache<'k,'v>() 
      let nslots = nslots
      let cache = new System.Collections.Generic.Dictionary<'k,'v*uint64 ref>()
      override this.TryGetValue k = 
        cache.Values |> Seq.iter (fun (v,lastAccess) -> lastAccess := !lastAccess + 1UL )    //naive
        match cache.TryGetValue k with
        |true,(v,_) -> Some v
        |false,_ -> None
      override this.Add k v =
        if cache.Count >= nslots then
          let vacate = cache |> Seq.maxBy (fun kvp -> snd kvp.Value)      // lru policy based on age
          cache.Remove vacate.Key |> ignore
        let slot = v,ref 0UL
        cache.Add(k,slot)
        v

module Realistic =
  /// Cache with pluggable policy to decide which item to vacate when the cache is full
  /// the vacatePolicy computes the total ordering on cache items from the tuple (key,value,age) such that the maximal value identifies the item to vacate next
  type N1PCache<'k,'v when 'k : equality>(nbooks,vacatePolicy:'k*'v*int64->int64) =
      inherit ICache<'k,'v>() 
      let cache = new System.Collections.Generic.Dictionary<'k,'v*int64 ref>()
      override this.TryGetValue k = 
        cache.Values |> Seq.iter (fun (v,age) -> age := !age + 1L)
        match cache.TryGetValue k with
        |true,(v,_) -> Some v
        |false,_ -> None
      override this.Add k v =
        if cache.Count >= nbooks then
          let tmp = cache |> Seq.map (fun kvp -> (kvp.Key,fst kvp.Value,!(snd kvp.Value)))
          let (vacate,_,_)= tmp |> Seq.maxBy vacatePolicy
          cache.Remove vacate |> ignore
        let slot = v,ref 0L
        cache.Add(k,slot)
        v

  module Policy =
    // some sample policies for vacating cache items
    let lru (k,v,age) = age             // vacate oldest
    let mru (k,v,age:int64) =  - age     // vacate youngest

    let big (k,v:string,age) = v.Length   // vacate biggest (for a hypothetical cache storing string values)

  /// Final N-Way cache capable of holding nshelves*nbooks items. 
  /// Items are internally arranged in a level-1 cache of nShelves each holding up to nBook items
  type NWayCache<'k,'v when 'k : equality>(nshelves,nbooks,vacatePolicy) =
      inherit ICache<'k,'v>() 

      let shelves = Array.init nshelves (fun x -> new N1PCache<'k,'v>(nbooks,vacatePolicy))
      let bucketFor k = shelves.[k.GetHashCode()%nshelves]
      override this.TryGetValue k = (bucketFor k).TryGetValue k
      override this.Add k v = (bucketFor k).Add k v
