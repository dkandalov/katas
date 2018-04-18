import luamidi.*
import kotlinx.cinterop.*
import kotlin.system.*

val midiDevice = rtmidi_out_create_default()

fun main(args: Array<String>) {
    if (args.size < 1) {
        println("Please specify Lua file as command line argument.")
        exitProcess(-1)
    }

    val portCount = rtmidi_get_port_count(midiDevice)
    if (portCount == 0) {
        println("No available midi ports")
        exitProcess(-1)
    }
    rtmidi_open_port(midiDevice, 0, "")

    val L = luaL_newstate()!!
    luaL_openlibs(L)

    lua_pushcclosure(L, staticCFunction { luaState: CPointer<lua_State>? ->
        midi_send(luaState!!)
        0
    }, 0)
    lua_setglobal(L, "midi_send")

    luaL_loadstring(L, "song = require 'notation'")
    lua_pcallk(L, 0, LUA_MULTRET, 0, 0, null).handleError(L)

    luaL_loadfilex(L, args[0], null)
    lua_pcallk(L, 0, LUA_MULTRET, 0, 0, null).handleError(L)

    luaL_loadstring(L, "song.go()")
    lua_pcallk(L, 0, LUA_MULTRET, 0, 0, null).handleError(L)

    lua_close(L)
}

fun midi_send(L: CPointer<lua_State>) {
    val status = lua_tonumberx(L, -3, null).toByte()
    val data1 = lua_tonumberx(L, -2, null).toByte()
    val data2 = lua_tonumberx(L, -1, null).toByte()

    val message = cValuesOf(status, data1, data2)
    rtmidi_out_send_message(midiDevice, message, 3)
}

fun Int.handleError(L: CPointer<lua_State>) {
    if (this != 0) {
        println("Error: " + lua_tolstring(L, -1, null)?.toKString())
        exitProcess(-1)
    }
}