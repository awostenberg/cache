// Learn more about F# at http://fsharp.org/. See the 'F# Tutorial' project
// for more guidance on F# programming.

// evaluate these expressions in visual studio F# interactive to try them out live and observe behavior
#load "Cache.fs"
open Cache.Realistic

module Examples =
  let square n =
    printfn "computing square of %d" n
    n*n

  let sqLru = Cache.Realistic.memoizeWithNWayCache square {nshelves=1;nbooks=10} Policy.lru 

  let x = [1..10] |> List.map sqLru
  let x1 = [1..10] |> List.map sqLru
  let x2 = sqLru 11     // will compute square of 11 and should vacate cached 1
  let x3 = sqLru 11     // should use cache
  let x4 = sqLru 1      // should recalculate and kick out 2

  let sqMru = cacheFor Policy.mru square
  let x' = [1..10] |> List.map sqMru   // load up cache
  let x1' = [1..10] |> List.map sqMru // from cache
  let x2' = sqMru 11      // should cache 11 and vacate the most recently used -- #10
  let x4' = sqMru 10      // should recalculate


  // sample policies for a hypothetial cache of type ImageTile showing how a policy could dictate what to vacate 
  // based on properties of the stored value and not mere age in cache
  type GeoLocation={lat:float;lon:float;zoom:int}
  type ImageTile = {location:GeoLocation;income:int;imageSize:int}
  let rich (k,v:ImageTile,age) = int64 v.income         // vacate richest
  let poor (k,v:ImageTile,age) = int64 -v.income        // vacate poorest
  let pig (k,v:ImageTile,age) = int64 v.imageSize       // vacate biggest 


  let renderTile geolocation =
    printfn "expensive computation to fetch a big image tile at %A " geolocation
    // made update data -- income proportional to longitude; image size related to zoom level
    {location=geolocation;income=geolocation.lon|>abs|>int;imageSize=geolocation.zoom}

  let boulder zoom = {lat=40.;lon= -105.;zoom=zoom}
  let samples = [1..10] |> List.map boulder


  let renderTileMem = cacheFor pig renderTile

  let g = samples |> List.map renderTileMem     // should calculate
  let g1 = samples |> List.map renderTileMem    // should hit cache
  let g2 = renderTileMem (boulder 99)           // should vacate item#10 in cache because it's the biggest (size proportional to zoom)
  let g3 = renderTileMem (boulder 10)           // should trigger recalc on 10
  let g4 = renderTileMem (boulder 9)            // should still be in cache
  let g5 = renderTileMem (boulder 88)           // should vacate 99 -- age in cache has nothing to do with "pig" policy
