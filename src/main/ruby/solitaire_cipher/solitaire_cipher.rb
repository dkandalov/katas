VALUE_OF_A = 64

def string_to_ints s
  uppercase_letters = s.gsub(/[^\w]/, "").upcase
  padded_uppercase_letters = uppercase_letters + "X" * ((5 - uppercase_letters.size % 5) % 5)
  padded_uppercase_letters.bytes.to_a.map { |i| i - VALUE_OF_A }
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
encrypted = encrypt(message, keystream)
p encrypted
decrypted = decrypt(encrypted, keystream)
p decrypted


def key_deck deck
  # do nothing for now
end

def move_down card, times, deck
  i = deck.index(card)
  new_i = (i + times) % deck.size
  new_i = new_i + 1 if (new_i < i)

  deck.delete_at(i)
  deck.insert(new_i, card)

  deck
end

def triple_cut deck
  before_first_jack = deck.take_while { |card| card != "A" and card != "B" }
  after_last_jack = deck.reverse.take_while { |card| card != "A" and card != "B" }.reverse
  between_jacks = deck[before_first_jack.size.. deck.size - after_last_jack.size]
  deck.clear
  deck.push(* after_last_jack + between_jacks + before_first_jack)
end

def count_cut deck
  count = deck.last
  count = 53 if count == "A" or count == "B"
  cards = deck.first(count)
  deck[0...count] = nil
  deck.insert(-2, *cards)
  deck
end

def output_letter deck
  count = deck.first
  count = 53 if count == "A" or count == "B"
  if ["A", "B"].include? deck[count]
    nil
  else
    deck[count] % 26
  end
end

def generate_keystream deck, size
  key = []
  while key.size < size do
    move_down "A", 1, deck
    move_down "B", 2, deck
    triple_cut deck
    count_cut deck
    letter = output_letter(deck)
    key << letter unless letter.nil?
  end
  ints_to_string key
end

deck = (1..52).to_a << "A" << "B"
p generate_keystream deck, 20 # expected D (4)  W (49)  J (10)  Skip Joker (53)  X (24)  H (8)  Y (51)  R (44)  F (6)  D (4)  G (33)
# actual "DWJDRAJHTOFADOIFSDBE"
