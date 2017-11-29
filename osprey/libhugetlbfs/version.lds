VERS_1.0 {
	global:
		gethugepagesize;
		hugetlbfs_test_path;
		hugetlbfs_find_path;
		hugetlbfs_unlinked_fd;
		__setup_hugepage;
		setup_libhugetlbfs;
	local:
		*;
};
