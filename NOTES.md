## Emulator flags for maximum process heap

- `+hmax` Size
    Sets the default maximum heap size of processes to the size Size. Defaults to 0, which means that no maximum heap size is used. For more information, see process_flag(max_heap_size, MaxHeapSize).

- `+hmaxel` true|false
    Sets whether to send an error logger message or not for processes reaching the maximum heap size. Defaults to true. For more information, see process_flag(max_heap_size, MaxHeapSize).

- `+hmaxk` true|false
    Sets whether to kill processes reaching the maximum heap size or not. Default to true. For more information, see process_flag(max_heap_size, MaxHeapSize).
