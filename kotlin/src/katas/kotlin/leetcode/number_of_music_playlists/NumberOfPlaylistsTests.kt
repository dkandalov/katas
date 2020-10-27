package katas.kotlin.leetcode.number_of_music_playlists

import datsok.shouldEqual
import org.junit.jupiter.api.Test

//
// https://leetcode.com/problems/number-of-music-playlists
//
// Your music player contains N different songs and she wants to listen to L (not necessarily different) songs during your trip.
// You create a playlist so that:
//  - Every song is played at least once
//  - A song can only be played again only if K other songs have been played
// Return the number of possible playlists. As the answer can be very large, return it modulo 10^9 + 7.
// Note: 0 <= K < N <= L <= 100
//
// Example 1:
// Input: N = 3, L = 3, K = 1
// Output: 6
// Explanation: There are 6 possible playlists. [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1].
//
// Example 2:
// Input: N = 2, L = 3, K = 0
// Output: 6
// Explanation: There are 6 possible playlists. [1, 1, 2], [1, 2, 1], [2, 1, 1], [2, 2, 1], [2, 1, 2], [1, 2, 2]
//
// Example 3:
// Input: N = 2, L = 3, K = 1
// Output: 2
// Explanation: There are 2 possible playlists. [1, 2, 1], [2, 1, 2]
//

fun numMusicPlaylists(n: Int, l: Int, k: Int): Int {
    return musicPlaylists(n, l, k).size
}

fun musicPlaylists(songCount: Int, playlistSize: Int, noRepeat: Int): List<Playlist> {
    return musicPlaylists_(songCount, playlistSize, noRepeat)
        .filter { it.isValid(songCount, playlistSize, noRepeat) }
        .sortedBy { it.toString() }
}

private fun musicPlaylists_(songCount: Int, playlistSize: Int, noRepeat: Int): List<Playlist> {
    if (songCount == 0 || playlistSize == 0) return listOf(emptyList())
    return (musicPlaylists_(songCount - 1, playlistSize - 1, noRepeat) + musicPlaylists_(songCount, playlistSize - 1, noRepeat))
        .flatMap { playlist -> playlist.insertAtAllPositions(songCount) }
        .distinct()
}

private fun Playlist.isValid(songCount: Int, playlistSize: Int, noRepeat: Int): Boolean {
    return size == playlistSize &&
        containsAll((1..songCount).toSet()) &&
        (noRepeat == 0 || windowed(size = noRepeat + 1).none { it.allItemsAreEqual() })
}

private fun Playlist.insertAtAllPositions(song: Int): List<Playlist> {
    val playlist = ArrayList(this)
    return (0..size).map { index ->
        ArrayList(playlist).also { it.add(index, song) }
    }
}

private fun Playlist.allItemsAreEqual(): Boolean =
    size <= 1 || this.all { it == first() }

typealias Playlist = List<Int>

class NumberOfPlaylistsTests {
    @Test fun `some examples`() {
        musicPlaylists(songCount = 3, playlistSize = 3, noRepeat = 1).toString() shouldEqual
            "[[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]"

        musicPlaylists(songCount = 2, playlistSize = 3, noRepeat = 0).toString() shouldEqual
            "[[1, 1, 2], [1, 2, 1], [1, 2, 2], [2, 1, 1], [2, 1, 2], [2, 2, 1]]"

        musicPlaylists(songCount = 2, playlistSize = 3, noRepeat = 1).toString() shouldEqual
            "[[1, 2, 1], [2, 1, 2]]"
    }
}