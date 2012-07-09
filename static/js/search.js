var Search = {
    execute: function(qstring) {
        var search = this;
        var url    = "/search/j/" + encodeURIComponent(qstring);

        $.getJSON(url, function(data) {
            search.display(data);
        });
    },

    display: function(results) {
        var html = "";

        $.each(results, function(id, result) {
            html += '<div class="result">'
                  + '<h3><a href="/posts/' + result.slug + '/">' + result.title + "</a></h3>"
                  + '<div class="result-excerpt">' + result.excerpt + '</div>'
                  + '</div>';
        });

        this.results.fadeOut('fast', function() {
            $(this).html(html).fadeIn('fast');
        });
    },

    attach: function() {
        this.search  = $('#search');
        this.results = $('#results');

        var minLen = 3;

        // store the current content so we can restore it when the
        // search box is emptied (drops under minLen)
        this.initial_html = this.results.html();

        // if we're rendering from a Back action, there might be an
        // ongoing search we should reexecute on load.
        var val = this.search.val();

        if (val && val.length >= minLen) {
            this.execute(val);
        }

        var search = this; // keep context in callback

        this.search.keyup(function() {
            $this = $(this);

            var newVal = $this.val();
            var oldVal = $this.data('old-value');

            if (newVal != oldVal) {
                if (newVal.length >= minLen) {
                    search.execute(newVal);
                }
                else if (oldVal && oldVal.length >= minLen) {
                    // empty after previous search, put the listing back
                    search.results.fadeOut('fast', function() {
                        $(this).html(search.initial_html).fadeIn('fast');

                        // put the table links back, TODO: make the initial
                        // click handler live instead
                        $('tbody.link tr').click(function() {
                            window.location = $(this).find('a').attr('href');
                        }).hover(function() {
                            $(this).toggleClass('hover')
                        });
                    });
                }
            }

            $this.data('old-value', newVal);
        });
    }
};
