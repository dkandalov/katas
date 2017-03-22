package katas.java.sort.radix;

import org.junit.Test;

import java.util.List;
import java.util.function.Function;

// https://probablydance.com/2016/12/27/i-wrote-a-faster-sorting-algorithm/
public class SkaSort {
    @Test
    public void sortList() {
        
    }

    private static class PartitionInfo {
        int count;
        int offset;
        int next_offset;
    }

    private static <T> void ska_byte_sort(List<T> list, Function<T, Integer> extract_key)
    {
        PartitionInfo[] partitions = new PartitionInfo[256];
        for (T it : list) {
            ++partitions[extract_key.apply(it)].count;
        }
        int[] remaining_partitions = new int[256];
        int total = 0;
        int num_partitions = 0;
        for (int i = 0; i < 256; ++i) {
            int count = partitions[i].count;
            if (count > 0) {
                partitions[i].offset = total;
                total += count;
                remaining_partitions[num_partitions] = i;
                ++num_partitions;
            }
            partitions[i].next_offset = total;
        }
        
//        for (uint8_t * last_remaining = remaining_partitions + num_partitions, * end_partition = remaining_partitions + 1; last_remaining > end_partition;)
//        {
//            last_remaining = custom_std_partition(remaining_partitions, last_remaining, [&](uint8_t partition)
//            {
//                size_t & begin_offset = partitions[partition].offset;
//                size_t & end_offset = partitions[partition].next_offset;
//                if (begin_offset == end_offset)
//                    return false;
//
//                unroll_loop_four_times(begin + begin_offset, end_offset - begin_offset, [partitions = partitions, begin, &extract_key, sort_data](It it)
//                {
//                    uint8_t this_partition = extract_key(*it);
//                    size_t offset = partitions[this_partition].offset++;
//                    std::iter_swap(it, begin + offset);
//                });
//                return begin_offset != end_offset;
//            });
//        }
    }
}
