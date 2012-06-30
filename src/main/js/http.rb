require 'webrick'

root = File.expand_path './'
server = WEBrick::HTTPServer.new :Port => 8000, :DocumentRoot => root
server.start