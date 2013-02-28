import unittest


def binary_search(value, list, shift=0):
    if not list:
        return -1

    midPos = len(list) / 2
    if value == list[midPos]:
        return shift + midPos
    elif value < list[midPos]:
        return binary_search(value, list[:midPos], shift)
    else:
        return binary_search(value, list[midPos + 1:], shift + midPos + 1)


class TestParsing(unittest.TestCase):
    def test_binary_search(self):
        self.assertEqual(-1, binary_search(1, []))

        list = [1]
        self.assertEqual(-1, binary_search(0, list))
        self.assertEqual(0, binary_search(1, list))
        self.assertEqual(-1, binary_search(2, list))

        list = [1, 2]
        self.assertEqual(-1, binary_search(0, list))
        self.assertEqual(0, binary_search(1, list))
        self.assertEqual(1, binary_search(2, list))
        self.assertEqual(-1, binary_search(3, list))

        list = [1, 2, 3]
        self.assertEqual(-1, binary_search(0, list))
        self.assertEqual(0, binary_search(1, list))
        self.assertEqual(1, binary_search(2, list))
        self.assertEqual(2, binary_search(3, list))
        self.assertEqual(-1, binary_search(4, list))

        list = [1, 2, 3, 4]
        self.assertEqual(-1, binary_search(0, list))
        self.assertEqual(0, binary_search(1, list))
        self.assertEqual(1, binary_search(2, list))
        self.assertEqual(2, binary_search(3, list))
        self.assertEqual(3, binary_search(4, list))
        self.assertEqual(-1, binary_search(5, list))

        self.assertEqual(
            [-1, 0, 1, 2, 3, 4, -1],
            [binary_search(value, [1, 2, 3, 4, 5]) for value in [0, 1, 2, 3, 4, 5, 6]])


if __name__ == '__main__':
    unittest.main()
