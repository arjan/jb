require(["jquery", "lib/knockout"], function($, ko) {
    var self = this;
    window.x = self;
    self.state = ko.observable({});
    self.queue = ko.observableArray();

    function api(method, data) {
        ws.send(JSON.stringify({cmd: method, data: data}));
    }
    
    var ws = new WebSocket('ws://' + document.location.host + '/socket');
    ws.onopen = function() {
        console.log('opened');
    };
    ws.onmessage = function(m) {
        var msg = JSON.parse(m.data);
        if (msg.event == 'status') {
            self.state(msg.data);
        } else if (msg.event == 'queue') {
            self.queue.removeAll();
            self.queue(msg.data);
        } else if (typeof msg.cmd == 'string') {
            // WS command reply, ignore
        } else {
            console.log('???');
            console.log(msg);
        }
    };

    setInterval(function() {
        ws.send('status');
    }, 1000);

    
    self.calculateProgressWidth = function() {
        if (self.state().track == null) return '0px';
            
        var total = $('#player .progress').width();
        return Math.floor(total * (self.state().pos / self.state().track.duration)) + 'px';
    };

    self.seekEvent = function(_, e) {
        if (self.state().track == null) return;

        var el = $('#player .progress');
        var d = (e.pageX - el.offset().left) / el.width();
        var ms = Math.floor(d * self.state().track.duration);
        console.log(ms);
        self.seek(ms);
    };

    self.formatTime = function(t) {
        t = Math.ceil(t / 1000);
        var r = t % 60;
        if (r < 10) r = '0' + r;
        return Math.floor(t / 60) + ':' + r;
    };

    self.queueDialog = function() {
        var link = self.prompt('Enter spotify URI');
        if (!link) return;
        api('queue_add', {link: link});
    };
    
    
    self.togglePause = function() {
        api('toggle_pause');
    };

    self.stop = function() {
        api('stop');
    };
    
    self.next = function() {
        api('next');
   };

    self.seek = function(t) {
        api('seek', {t: t});
    };

    ko.cleanNode(document.body);
    ko.applyBindings(self, document.body);
});
