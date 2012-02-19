VALUE_OF_A = 64

def string_to_ints s
  uppercase_letters = s.gsub(/[^\w]/, "").upcase
  padded_uppercase_letters = uppercase_letters + "X" * ((5 - s.size % 5) % 5)
  padded_uppercase_letters.bytes.to_a.map{|i| i - VALUE_OF_A}
end

def ints_to_string ints
  ints.map { |i| (i + 64).chr }.join("")
end

def sum_ints ints1, ints2
  result = []
  ints1.each_with_index { |i1, i2| result << i1 + ints2[i2 % ints2.size] }
  result.map { |i| i % 26 }
end

def subtract_ints ints1, ints2
  result = []
  ints1.each_with_index { |i1, i2| result << i1 + 26 - ints2[i2 % ints2.size] }
  result.map { |i| i % 26 }
end

def encrypt message, keystream
  msg_ints = string_to_ints message
  keystream_ints = string_to_ints keystream
  ints_to_string(sum_ints(msg_ints, keystream_ints))
end

def decrypt message, keystream
  msg_ints = string_to_ints message
  keystream_ints = string_to_ints keystream
  ints_to_string(subtract_ints(msg_ints, keystream_ints))
end

message = "Code in Ruby, live longer!"
keystream = "DWJXHYRFDGTMSHPUURXJ" # solitaire keystream
p string_to_ints(message).size, string_to_ints(keystream).size
encrypted = encrypt(message, keystream)
p encrypted.size
decrypted = decrypt(encrypted, keystream)
p decrypted.size
