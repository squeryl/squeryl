/*******************************************************************************
 * Copyright 2010 Maxime Lévesque
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 ***************************************************************************** */
package org.squeryl.demos;

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.adapters.H2Adapter
import org.squeryl.{Query, Session, KeyedEntity, Schema}
import java.sql.SQLException
import org.squeryl.dsl.GroupWithMeasures
// The root object of the schema. Inheriting KeyedEntity[T] is not mandatory
// it just makes primary key methods available (delete and lookup) on tables.
class MusicDbObject extends KeyedEntity[Long] {
  val id: Long = 0
}

case class Artist(val name:String) extends MusicDbObject {

  // this returns a Query[Song] which is also an Iterable[Song] :
  def songs = from(MusicDb.songs)(s => where(s.artistId === id) select(s))

  def newSong(title: String, filePath: Option[String], year: Int) =
    MusicDb.songs.insert(new Song(title, id, filePath, year))
}

// Option[] members are mapped to nullable database columns,
// otherwise they have a NOT NULL constraint.
class Song(val title: String, val artistId: Long, val filePath: Option[String], val year: Int) extends MusicDbObject {

  // IMPORTANT : currently classes with Option[] members *must* provide a zero arg
  // constructor where every Option[T] member gets initialized with Some(t:T).
  // or else Squeryl will not be able to reflect the type of the field, and an exception will
  // be thrown at table instantiation time.
  def this() = this("", 0, Some(""),0)

  // the schema can be imported in the scope, to lighten the syntax :
  import MusicDb._
  
  // An alternative (shorter) syntax for single table queries :
  def artist = artists.where(a => a.id === artistId).single

  // Another alternative for lookup by primary key, since Artist is a
  // KeyedEntity[Long], it's table has a lookup[Long](k: Long)
  // method available :
  def lookupArtist = artists.lookup(artistId)
}

class Playlist(val name: String, val path: String) extends MusicDbObject {

  import MusicDb._

  // a two table join : 
  def songsInPlaylistOrder =
    from(playlistElements, songs)((ple, s) =>
      where(ple.playlistId === id and ple.songId === s.id)
      select(s)
      orderBy(ple.songNumber asc)
    )

  def addSong(s: Song) = {

    // Note how this query can be implicitly converted to an Int since it returns
    // at most one row, this applies to all single column aggregate queries with no groupBy clause.
    // The nvl function in this example changed the return type to Int, from
    // Option[Int], since the 'max' function (like all aggregates, 'count' being the only exception).
    val nextSongNumber: Int =
      from(playlistElements)(ple =>
        where(ple.playlistId === id)
        compute(nvl(max(ple.songNumber), 0))
      )    
    
    playlistElements.insert(new PlaylistElement(nextSongNumber, id, s.id))
  }

  def removeSong(song: Song) =
    playlistElements.deleteWhere(ple => ple.songId === song.id)

  def removeSongOfArtist(artist: Artist) =
    playlistElements.deleteWhere(ple =>
      (ple.playlistId === id) and
      (ple.songId in from(songsOf(artist.id))(s => select(s.id)))
    )

  // New concept : a group query with aggregate functions return GroupWithMeasures[K,M]
  // where K and M are tuples whose members correspond to the group by list and compute list
  // respectively.
  def _songCountByArtistId: Query[GroupWithMeasures[Long,Long]] =
    from(artists, songs)((a,s) =>
      where(a.id === s.artistId)
      groupBy(a.id)
      compute(count)
    )

  // Queries are nestable just as they would in SQL
  def songCountForAllArtists  =
    from(_songCountByArtistId, artists)((sca,a) =>
      where(sca.key === a.id)
      select((a, sca.measures))
    )

  // Unlike SQL, a function that returns a query can be nested
  // as if it were a query, notice the nesting of 'songsOf'
  // allowing DRY persistence layers as reuse is enhanced.
  def latestSongFrom(artistId: Long) =
    from(songsOf(artistId))(s =>
      select(s)
      orderBy(s.id desc)
    ).headOption

  def songsOf(artistId: Long) =
    from(playlistElements, songs)((ple,s) =>
      where(id === ple.playlistId and ple.songId === s.id and s.artistId === artistId)
      select(s)
    )
}


class PlaylistElement(var songNumber: Int, var playlistId: Long, var songId: Long)


class Rating(var userId: Long, var appreciationScore: Int, var songId: Int)


object MusicDb extends Schema {

  val artists = table[Artist]
  val songs = table[Song]
  val playlists = table[Playlist]
  val playlistElements = table[PlaylistElement]
  val ratings = table[Rating]

  // drop (schema) is normaly protected... for safety, here we live dangerously !
  override def drop = super.drop
}



object KickTheTires {

  import MusicDb._

  def initSchema = {

    MusicDb.drop
    MusicDb.create    
  }

  def testWithH2 = {
    //A Squeryl session is a thin wrapper over a JDBC connection :
    Class.forName("org.h2.Driver");
    val session = Session.create(
      java.sql.DriverManager.getConnection("jdbc:h2:~/test", "sa", ""),
      //Currently there are adapters for Oracle, Postgres, MySql and H2 :
      new H2Adapter
    )
    test(session)
  }

  def test(session: Session) = using(session) {

    initSchema
    
    val herbyHancock = artists.insert(new Artist("Herby Hancock"))
    val ponchoSanchez = artists.insert(new Artist("Poncho Sanchez"))
    val mongoSantaMaria = artists.insert(new Artist("Mongo Santa Maria"))
    val theMeters = artists.insert(new Artist("The Meters"))

    val watermelonMan = herbyHancock.newSong("Watermelon Man", None, 1962)
    val besameMama = mongoSantaMaria.newSong("Besame Mama", Some("c:/MyMusic/besameMama.flac"), 1998)
    val freedomSound = ponchoSanchez.newSong("Freedom Sound", None, 1997)
    val funkifyYouLife = theMeters.newSong("Funkify Your Life", None,1969)
    val goodOldFunkyMusic = theMeters.newSong("Good old Funky Music", None, 1968)

    val funkAndLatinJazz = playlists.insert(new Playlist("Funk and Latin Jazz", "c:/myPlayLists/funkAndLatinJazz"))

    funkAndLatinJazz.addSong(watermelonMan)
    funkAndLatinJazz.addSong(besameMama)
    funkAndLatinJazz.addSong(freedomSound)

    val decadeOf1960 = playlists.insert(new Playlist("1960s", "c:/myPlayLists/funkAndLatinJazz"))
    
    decadeOf1960.addSong(watermelonMan)
    decadeOf1960.addSong(funkifyYouLife)
    decadeOf1960.addSong(goodOldFunkyMusic)

    //Session.currentSession.setLogger(m => println(m))

    // Nesting a query in a where clause : 
    val songsFromThe60sInFunkAndLatinJazzPlaylist =
      from(songs)(s=>
        where(s.id in from(funkAndLatinJazz.songsInPlaylistOrder)(s2 => select(s2.id)))
        select(s)
      )

//    for(s <- songsFromThe60sInFunkAndLatinJazzPlaylist)
//      println(s.title + " : " + s.year)

    // Nesting in From clause :
    val songsFromThe60sInFunkAndLatinJazzPlaylist2 =
      from(funkAndLatinJazz.songsInPlaylistOrder)(s=>
        where(s.id === 123)
        select(s)
      )
    
    // Left Outer Join :
    var ratingsForAllSongs =
      from(songs, ratings)((s,r) =>
          select((s, leftOuterJoin(r, s.id === r.songId)))
      )

//    for(sr <- ratingsForAllSongs)
//      println(sr._1.title + " rating is " + sr._2.map(r => r.appreciationScore.toString).getOrElse("not rated"))


    update(songs)(s =>
      where(s.title === "Watermelon Man")
      set(s.title := "The Watermelon Man",
          s.year  := s.year plus 1)
    )

    for(s <- funkAndLatinJazz.songsOf(herbyHancock.id))
      println("herby " + s.title)
    
    val c = funkAndLatinJazz.removeSongOfArtist(herbyHancock)

    assert(c == 1, "expected 1, got " + c + "playList.id:" + funkAndLatinJazz.id + ", artist.id:" + herbyHancock.id)


    funkAndLatinJazz._songCountByArtistId.toList

    
    val q = funkAndLatinJazz.songCountForAllArtists

    //println(q.dumpAst)
    
    q.toList
  }
}
