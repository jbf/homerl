#include <telldus-core.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define DATA_LENGTH 64
#define MESSAGE_MAX_LEN 1024

void print_event(const char *protocol, const char *model, int id, int dataType, const char *value, int timestamp, int callbackId, void *context);
int write_fully(int len, char *buf);
const char *data_type_to_string(int data_type);

int main(int argc, char *argv[]);

int main(int argc, char *argv[]) {
    int id, len;
    char buf[DATA_LENGTH] = { 0, };

    id = tdRegisterSensorEvent((TDSensorEvent)print_event, NULL);

    while ((len = read(STDIN_FILENO, buf, DATA_LENGTH)) != 0);

    return 0;
}

void print_event(const char *protocol, const char *model, int id, int dataType, const char *value, int timestamp, int callbackId, void *context) {
    char buf[MESSAGE_MAX_LEN] = {'\0', };
    int len;
    char len_buf;

    // Prepare message
    len = snprintf(buf, MESSAGE_MAX_LEN, "{%d, %s, %s, %d}.", id, data_type_to_string(dataType), value, timestamp);

    if (len > MESSAGE_MAX_LEN) { // this wouldn't parse
        fprintf(stderr, "Message too long: ");
        // Log to stderr as well
        fprintf(stderr, "protocol=%s, \
model=%s, \
id=%d, \
dataType=%d, \
value=%s, \
timestamp=%d, \
callbackId=%d\
\n", protocol, model, id, dataType, value, timestamp, callbackId);
        fflush(stderr);
    } else { // send to other endpoint
        len_buf = (len >> 8) & 0xFF;
        write_fully(1, &len_buf);

        len_buf = len & 0xFF;
        write_fully(1, &len_buf);

        write_fully(len, buf);
    }
}

int write_fully(int len, char *buf) {
    int wrote, total = 0;

    do {
        if((wrote = write(STDOUT_FILENO, buf + total, len - total)) <= 0)
            return wrote;
        total += wrote;
    } while (total < len);

    return total;
}

const char *data_type_to_string(int data_type) {
    switch (data_type) {
        case TELLSTICK_TEMPERATURE:
            return "t";
        case TELLSTICK_HUMIDITY:
            return "h";
        default:
            return "unknown";
    }
}
