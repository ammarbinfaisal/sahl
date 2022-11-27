from typing import List

def merge(arr: List[int], left: int, mid: int, right: int):
    arr1 = []
    arr2 = []
    for i in range(left, mid + 1):
        arr1.append(arr[i])
    for i in range(mid + 1, right + 1):
        arr2.append(arr[i])
    i = 0
    j = 0
    k = left
    while i < len(arr1) and j < len(arr2):
        if arr1[i] < arr2[j]:
            arr[k] = arr1[i]
            i += 1
        else:
            arr[k] = arr2[j]
            j += 1
        k += 1
    while i < len(arr1):
        arr[k] = arr1[i]
        i += 1
        k += 1
    while j < len(arr2):
        arr[k] = arr2[j]
        j += 1
        k += 1    


def merge_sort(arr: List[int], left: int, right: int):
    if left < right:
        mid = (left + right) // 2
        merge_sort(arr, left, mid)
        merge_sort(arr, mid + 1, right)
        merge(arr, left, mid, right)
if __name__ == "__main__":
    arr = []
    for i in range(100000, 0, -1):
        arr.append(i)
    merge_sort(arr, 0, len(arr) - 1)
    print(arr)
