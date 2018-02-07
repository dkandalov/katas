extern "C"
{
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
}
#include "RtMidi.h"
static RtMidiOut midi;

int midi_send(lua_State* L)
{
  double status = lua_tonumber(L, -3);
  double data1 = lua_tonumber(L, -2);
  double data2 = lua_tonumber(L, -1);

  std::vector<unsigned char> message(3);
  message[0] = static_cast<unsigned char>(status);
  message[1] = static_cast<unsigned char>(data1);
  message[2] = static_cast<unsigned char>(data2);

  midi.sendMessage(&message);

  return 0;
}

int main(int argc, const char* argv[])
{
  if (argc < 1) return -1;

  int ports = midi.getPortCount();
  if (ports < 1) {
    printf("No available midi ports\n");
    return -1;
  }
  midi.openPort(0);

  lua_State* L = luaL_newstate();
  luaL_openlibs(L);

  lua_pushcfunction(L, midi_send);
  lua_setglobal(L, "midi_send");

  // luaL_dostring(L, "print('Hello world!')");
  int error = luaL_dofile(L, argv[1]);
  if (error != 0) {
    printf("Error executing Lua code:\n%s\n", lua_tostring(L, -1));
    return 1;
  }

  lua_close(L);
  return 0;
}
