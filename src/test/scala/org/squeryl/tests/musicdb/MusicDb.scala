package org.squeryl.tests.musicdb

import java.sql.SQLException
import org.squeryl.tests.QueryTester
import org.squeryl._
import dsl.{Measures, GroupWithMeasures}

class MusicDbObject extends KeyedEntity[Int] {
  var id: Int = 0
}

class Person(var firstName:String, var lastName: String) extends MusicDbObject

class Song(var title: String, var authorId: Int, var interpretId: Int, var cdId: Int) extends MusicDbObject

class Cd(var title: String, var mainArtist: Int, var year: Int) extends MusicDbObject


class MusicDb extends Schema with QueryTester {

  import org.squeryl.PrimitiveTypeMode._

  val songs = table[Song]

  val artists = table[Person]

  val cds = table[Cd]

  val testInstance = new {

    try {
      drop // *NOT* something to do in real life...
    }
    catch {
      case e:SQLException => {} 
    }

    create

    val herbyHancock = artists.insert(new Person("Herby", "Hancock"))
    val ponchoSanchez = artists.insert(new Person("Poncho", "Sanchez"))
    val mongoSantaMaria = artists.insert(new Person("Mongo", "Santa Maria"))
    val alainCaron = artists.insert(new Person("Alain", "Caron"))
    val hossamRamzy = artists.insert(new Person("Hossam", "Ramzy"))

    val congaBlue = cds.insert(new Cd("Conga Blue", ponchoSanchez.id, 1998))
    val   watermelonMan = songs.insert(new Song("Watermelon Man", herbyHancock.id, ponchoSanchez.id, congaBlue.id))
    val   besameMama = songs.insert(new Song("Besame Mama", mongoSantaMaria.id, ponchoSanchez.id, congaBlue.id))

    val freedomSoundAlbum = cds.insert(new Cd("Freedom Sound", ponchoSanchez.id, 1997))
    val   freedomSound = songs.insert(new Song("Freedom Sound", ponchoSanchez.id, ponchoSanchez.id, freedomSoundAlbum.id))
        

    val expectedSongCountPerAlbum = List((congaBlue.title,2), (freedomSoundAlbum.title, 1))
  }

  val basicSelectUsingWhereOnQueryable =
    artists.where(a=> a.id =? testInstance.mongoSantaMaria.id)

  val basicSelectUsingWhereOnQueryableNested =
    basicSelectUsingWhereOnQueryable.where(a=> a.id =? testInstance.mongoSantaMaria.id)
  
  lazy val poncho =
   From(artists)(a => 
    ~:Where(a.firstName =? "Poncho") Select(a)
   )

  def selfJoinNested3Level = 
    From(
      From(
        From(artists)(a => ~:Where(a.id =? testInstance.ponchoSanchez.id) Select(a))
      )(a => ~:Select(a))
    )(a => ~:Select(a))

  def selfJoinNested4LevelPartialSelect =
    From(selfJoinNested3Level)(a => ~:Where(a.id.~ > -1) Select(a.lastName))

  lazy val songsFeaturingPoncho =
    From(songs, artists)((s,a) =>
    ~:Where(a.firstName =? "Poncho" and s.interpretId =? a.id)
      Select(s)
      OrderBy(s.title, a.id Desc)
    )

  lazy val songsFeaturingPonchoNestedInWhere =
    From(songs, artists)((s,a) =>
    ~:Where(
        s.interpretId in From(artists)(a => ~:Where(a.firstName =? "Poncho") Select(a.id))
      )
      Select(s)
      OrderBy(s.title Asc)
    ).Distinct

  lazy val yearOfCongaBluePlus1 =
    From(cds)(cd =>
    ~:Where(cd.title =? testInstance.congaBlue.title)
      Select(Value(cd.year.~ + 1))
    )
  
  def songCountPerAlbum(cds: Queryable[Cd]) =
    From(cds, songs)((cd, song) =>
    ~:Where(song.cdId =? cd.id)
      GroupBy(cd.title) Compute(Count)
      OrderBy(cd.title)
    )

  lazy val songCountPerAlbumFeaturingPoncho = songCountPerAlbum(
      From(songs, artists, cds)((s, a, cd) =>
      ~:Where(a.firstName =? "Poncho" and s.interpretId =? a.id and s.cdId =? cd.id)
        Select(cd)
      ).Distinct
    )

  lazy val songsFeaturingPonchoNestedInFrom =
    From(songs, poncho)((s,a) =>
    ~:Where(s.interpretId =? a.id)
      Select((s,a.firstName))
      OrderBy(s.title)
    )

  def songCountPerAlbumId(cds: Queryable[Cd]) =
    From(cds, songs)((cd, song) =>
    ~:Where(song.cdId =? cd.id)
      GroupBy(cd.id) Compute(Count)
    )

  lazy val songCountPerAlbumIdJoinedWithAlbum  =
    From(songCountPerAlbumId(cds), cds)((sc, cd) =>
    ~:Where(sc.key =? cd.id)
      Select((cd.title, sc.measures))
      OrderBy(cd.title)
    )

  def songCountPerAlbumIdJoinedWithAlbumNested =
    From(songCountPerAlbumIdJoinedWithAlbum)(q =>
    ~:Select(q)
    )

  //TODO: list2Queryable conversion using 'select x0 as x from dual union ...'
  def artistsInvolvedInSongs(songIds: List[Int]) =
    From(
      From(songs)(s =>
      ~:Where((s.authorId in songIds) or (s.interpretId in songIds))
        Select(s)
      ),
      artists
    )((s,a) =>
    ~:Where(s.authorId =? a.id or s.interpretId =? a.id)
      Select(a)
      OrderBy(a.lastName Desc)
    ).Distinct

  def countCds(cds: Queryable[Cd]) =
    From(cds)(c => ~:Compute(Count))

  def countCds2(cds: Queryable[Cd]) = cds.Count    

  def avgSongCountForAllArtists =
    From(
      From(artists, songs)((a,s) =>
      ~:Where(s.authorId =? a.id)
        GroupBy(a.id) Compute(Count)
      )
    )((sonCountPerArtist) =>
      ~:Compute(Avg(sonCountPerArtist.measures))
    )

  def test1 = {
    import testInstance._
    //logQueries = true
    //doNotExecute = true

    //activateWorkbenchMode

    working
  }

  def working = {
    import testInstance._

    testDynamicQuery1
    
    testDynamicQuery2
    
    testKeyedEntityImplicitDelete
    
    testKeyedEntityImplicitLookup
    
    validateQuery('basicSelectUsingWhereOnQueryable, basicSelectUsingWhereOnQueryable, (a:Person)=>a.id, List(mongoSantaMaria.id))

    validateQuery('basicSelectUsingWhereOnQueryableNested, basicSelectUsingWhereOnQueryableNested, (a:Person)=>a.id, List(mongoSantaMaria.id))
    
    validateQuery('poncho, poncho, (a:Person)=>a.lastName, List(ponchoSanchez.lastName))

    val ponchoSongs = List(besameMama.title, freedomSound.title, watermelonMan.title)

    validateQuery('songsFeaturingPoncho, songsFeaturingPoncho, (s:Song)=>s.title,
      ponchoSongs)

    validateQuery('songsFeaturingPonchoNestedInWhere, songsFeaturingPonchoNestedInWhere, (s:Song)=>s.title,
      ponchoSongs)

    validateQuery('songCountPerAlbum, songCountPerAlbum(cds),
      (g:GroupWithMeasures[String,Long]) => (g.key,g.measures),
      expectedSongCountPerAlbum)
    
    validateQuery('yearOfCongaBluePlus1, yearOfCongaBluePlus1, identity[Int], List(1999))

    validateQuery('songCountPerAlbumFeaturingPoncho, songCountPerAlbumFeaturingPoncho,
      (g:GroupWithMeasures[String,Long]) => (g.key,g.measures),
      List((congaBlue.title,2), (freedomSoundAlbum.title, 1))
    )
    
    validateQuery('songsFeaturingPonchoNestedInFrom, songsFeaturingPonchoNestedInFrom,
      (t:(Song,String)) => (t._1.id,t._2),
      List((besameMama.id, ponchoSanchez.firstName),
           (freedomSound.id, ponchoSanchez.firstName),
           (watermelonMan.id, ponchoSanchez.firstName)))

    validateQuery('selfJoinNested4LevelPartialSelect, selfJoinNested4LevelPartialSelect,
      identity[String], List(ponchoSanchez.lastName))

    validateQuery('selfJoinNested3Level, selfJoinNested3Level, (a:Person)=>a.lastName, List(ponchoSanchez.lastName))

    validateQuery('songCountPerAlbumIdJoinedWithAlbum, songCountPerAlbumIdJoinedWithAlbum,
      (t:(String,Long)) => (t._1,t._2),
      expectedSongCountPerAlbum)

   validateQuery('songCountPerAlbumIdJoinedWithAlbumNested, songCountPerAlbumIdJoinedWithAlbumNested,
      (t:(String,Long)) => (t._1,t._2),
      expectedSongCountPerAlbum)

    validateQuery('artistsInvolvedInSongsm, artistsInvolvedInSongs(List(watermelonMan.id)),
      (a:Person) => a.id, List(ponchoSanchez.id, herbyHancock.id))

    validateQuery('countCds, countCds(cds), (m:Measures[Long]) => m.measures, List(2))

    validateQuery('countCds2, countCds2(cds), identity[Long], List(2))

    validateScalarQuery1

    validateScalarQueryConversion1

    testUpdate1
  }

  def validateScalarQuery1 = {
    val cdCount: Long = countCds2(cds)
    assert(cdCount == 2, "exprected 2, got " + cdCount + " from " + countCds2(cds))

    println("validateScalarQuery1 passed.")
  }

  def validateScalarQueryConversion1 = {
    //val q:Query[Measures[Double]] = avgSongCountForAllArtists
    //q.dumpAst
    val d:Option[Double] = avgSongCountForAllArtists
    //println("d=" + d)
    assert(d.get == 1.0, "expected " + 1.0 +"got "  +d)
    println("validateScalarQueryConversion1 passed.")
  }

  def testUpdate1 = {
    import testInstance._

    var ac = artists.where(a=> a.id =? alainCaron.id).single
    ac.lastName = "Karon"
    artists.update(ac)
    ac = artists.where(a=> a.id =? alainCaron.id).single
    assert(ac.lastName == "Karon", 'testUpdate1 + " failed, expected Karon, got " + ac.lastName)
    println('testUpdate1 + " passed.")
  }

  def testKeyedEntityImplicitLookup = {
    import testInstance._

//    val p:KeyedEntity[Int] = ponchoSanchez
//    artists : Queryable[KeyedEntity[Int]]
//    artists : Updatable[KeyedEntity[Int]]
//    (artists : Queryable[KeyedEntity[Int]]) : KeyedEntityView[Int, Person]
//    (artists : Updatable[KeyedEntity[Int]]) : KeyedEntityTable[Int, Person]
//    val kev: KeyedEntityTable[Int, Person] = artists
//    val ket: KeyedEntityTable[Int, Person] = artists

    //view2KeyedEntityView(artists): KeyedEntityView[Int,Person]
    
    var ac = artists.lookup(alainCaron.id).get
    assert(ac.id == alainCaron.id, "expected " + alainCaron.id + " got " + ac.id)
    passed('testKeyedEntityImplicitLookup)
  }

  import testInstance._

  def testKeyedEntityImplicitDelete = {  

    val artistForDelete = artists.insert(new Person("Delete", "Me"))

    assert(artists.delete(artistForDelete.id), "delete returned false, expected true")

    assert(artists.lookup(artistForDelete.id) == None, "object still exist after delete")

    passed('testKeyedEntityImplicitDelete)
  }

  def inhibitedArtistsInQuery(inhibit: Boolean) =
    From(songs, artists.inhibitWhen(inhibit))((s,a) =>
    ~:Where(a.get.firstName =? "Poncho" and s.interpretId =? a.get.id)
      Select(s)
      OrderBy(s.title, a.get.id Desc)
    )  

  def testDynamicQuery1 = {

    val allSongs =
      From(songs)(s =>
      ~:Select(s)
        OrderBy(s.title)
      ).toList.map(s => s.id)

    val q = inhibitedArtistsInQuery(true)
    //println(q.dumpAst)
    val songsInhibited = q.toList.map(s => s.id)

    assert(allSongs == songsInhibited, "query inhibition failed, expected "+allSongs+", got " + songsInhibited)

    val songsNotInhibited = inhibitedArtistsInQuery(false)
    
    val ponchoSongs = List(besameMama.title, freedomSound.title, watermelonMan.title)

    validateQuery('songsNotInhibited, songsNotInhibited, (s:Song)=>s.title,
      ponchoSongs)
 
    passed('testDynamicQuery1)
  }

  def inhibitedSongsInQuery(inhibit: Boolean) =
    From(songs.inhibitWhen(inhibit), artists)((s,a) =>
    ~:Where(a.firstName =? "Poncho" and s.get.interpretId =? a.id)
      Select((s, a))
      OrderBy(s.get.title, a.id Desc)
    )

  def testDynamicQuery2 = {

    val q = inhibitedSongsInQuery(true)
    println(q.dumpAst)
    val t = q.single
    val poncho = t._2

    assert(ponchoSanchez.id == poncho.id, 'inhibitedSongsInQuery + " failed, expected " + ponchoSanchez.id + " got " + poncho.id)

    assert(t._1 == None, "inhibited table in query should have returned None, returned " + t._1)

    val songArtistsTuples = inhibitedSongsInQuery(false)

    val expected =
      From(songs, artists)((s,a) =>
      ~:Where(a.firstName =? "Poncho" and s.interpretId =? a.id)
        Select((s.id, a.id))
        OrderBy(s.title, a.id Desc)
      )
    
    validateQuery('inhibitedSongsInQuery, songArtistsTuples,  (t:(Option[Song],Person)) => (t._1.get.id, t._2.id),
      expected.toList
    )

    passed('testDynamicQuery2)
  }
}