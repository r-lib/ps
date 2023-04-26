
int ps__pid_exists(long pid);
void ps__raise_for_pid(long pid, char *msg);
extern pthread_t ps__main_thread;
int ps__nonblock_fcntl(int fd, int set);
