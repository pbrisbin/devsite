<?php

/*
 * pbrisbin 2010
 *
 */

/* Configuration */

// file path to the cache
$cache_file = './cache';

// server-relative path to a css file
$css_file = '/generic.css';

// valid users...     username      password
$valid_users = array('tbarnes'  => 'pass@word1',
                     'pbrisbin' => 'pass@word2');

/* Functions */

// load and write the cache file {{{
function load_cache() {
    global $cache_file, $new_contents;

    $new_contents = array();

    // load the cache file
    $cache_contents = file($cache_file, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    // make a copy to work with
    foreach ($cache_contents as $str) {
        $array = explode('|', $str);
        $new_contents[] = $array;
    }
}

function write_cache() {
    global $cache_file, $new_contents;

    $fh = fopen($cache_file, 'w');

    if ($fh) {
        // write out each cache entry
        foreach($new_contents as $array) {
            $str = implode('|', $array).PHP_EOL;
            fwrite($fh, $str);
        }
        fclose($fh);
    }
}
// }}}

// edit and save a single entry {{{
function edit_entry($_id) {
    global $new_contents;

    load_cache();

    list($songinfo, $album, $image, $url) = $new_contents[$_id];

    // display and edit box for the entry we're editing
    echo '<form action="?id='.$_id.'&save'.(isset($_GET['filter']) ? '&filter' : '').'" method="post">'.PHP_EOL;
    echo '    <label for="songinfo">Song info:</label><br />'.PHP_EOL;
    echo '    <input type="text" value="'.$songinfo.'" readonly id="songinfo" name="songinfo" size="100" /><br />'.PHP_EOL;
    echo '    <label for="album">Album name:</label><br />'.PHP_EOL;
    echo '    <input type="text" value="'.$album.'" id="album" name="album" size="100" /><br />'.PHP_EOL;
    echo '    <label for="image">Image path or url:</label><br />'.PHP_EOL;
    echo '    <input type="text" value="'.$image.'" id="image" name="image" size="100" /><br />'.PHP_EOL;
    echo '    <label for="url">Product page url:</label><br />'.PHP_EOL;
    echo '    <input type="text" value="'.$url.'" id="url" name="url" size="100" /><br />'.PHP_EOL;
    echo '    <input type="submit" value="Submit" />'.PHP_EOL;
    echo '    <input type="reset" />'.PHP_EOL;
    echo '</form>'.PHP_EOL;
    echo '<p>To cancel, click Back in your browser.</p>'.PHP_EOL;
}

function save_entry($_id, $_array) {
    global $new_contents;

    load_cache();

    // simply replace the element with the posted values and write it 
    // out
    $new_contents[$_id] = $_array;

    write_cache();
}
// }}}

// display initial page {{{
function display_cache($_filter = false) {
    global $new_contents;

    load_cache();

    // the overall table
    echo '<table>'.PHP_EOL;
    echo '<tr class="heading"><td>Entry</td><td>Album</td><td>Image</td><td>Edit</td></tr>'.PHP_EOL;

    $i = 0;
    foreach ($new_contents as $entry) {
        if (!isset($_GET['filter']) || $entry[1] == '_NOT_FOUND_') {
            print_entry($i, $entry);
        }

        // $i will always be the element's position in the file 
        // regardless of how the entries are display -- this keeps the 
        // edit/save logic consistent in filtered/non-filtered modes
        $i++;
    }

    echo '</table>'.PHP_EOL;
}

// inside the loop, each row is printed
function print_entry($_id, $_entry) {
    list($songinfo, $album, $image, $_) = $_entry;
    $edit = '<a href="?id='.$_id.'&edit'.(isset($_GET['filter']) ? '&filter' : '').'">edit</a>';

    echo '<tr>';
    echo '<td>'.$songinfo.'</td>';
    echo '<td>'.$album.'</td><td>'.$image.'</td>';
    echo '<td>'.$edit.'</td>';
    echo '</tr>'.PHP_EOL;
}
// }}}

// page header/footer {{{
function page_header() {
    global $css_file;

    echo '<!doctype html>';
    echo '<html lang="en">';
    echo '<head>';
    echo '<link type="text/css" rel="stylesheet" href="'.$css_file.'" />';
    echo '</head>';
    echo '<body>';
    echo '<h1>Coverfetch cache administation</h1>';
    echo '<hr />'.PHP_EOL;

    // toggle the filter
    if (isset($_GET['filter'])) {
        echo '<p><a href="?">Disable</a> <em>Not Found</em> filter</p>';
    }
    else {
        echo '<p><a href="?filter">Enable</a> <em>Not Found</em> filter</p>';
    }
}

function page_footer() {
    echo '</body>';
    echo '</html>'.PHP_EOL;
}
// }}}

// authentication {{{
function user_is_valid($_user, $_pass) {
    global $valid_users;

    if (isset($valid_users[$_user]) && ($valid_users[$_user] == $_pass)) {
        return true;
    }
    else {
        return false;
    }
}

function authenticate() {
    global $css_file;

    header('WWW-Authenticate: Basic ream="Coverfetch"');
    header('HTTP/1.0 401 Unauthorized');

    page_header();
    echo '<p>You must be authorized to use cache administration.</p>';
    page_footer();
    exit;
}

// credentials not known
if (!isset($_SERVER['PHP_AUTH_USER'])) {
    authenticate();
}

// bad user/password
if (!user_is_valid($_SERVER['PHP_AUTH_USER'], $_SERVER['PHP_AUTH_PW'])) {
    authenticate();
}
// }}}

/* Page logic */

// edit page {{{
if (isset($_GET['edit'])) {
    $id = $_GET['id'];

    page_header();
    edit_entry($id); 
    page_footer();
}
// }}}

// save page {{{
else if (isset($_GET['save'])) {
    $id       = $_GET['id'];
    $songinfo = $_POST['songinfo'];
    $album    = $_POST['album'];
    $image    = $_POST['image'];
    $url      = $_POST['url'];

    if ($songinfo) {
        save_entry($id, array($songinfo, $album, $image, $url));
    }
    else {
        page_header();
        echo '<p>Invalid POST data.</p>';
        echo '<p><a href="?">Go back</a></p>';
        page_footer();
    }

    list($req, $rest) = split('\?', $_SERVER['REQUEST_URI'], 2);
    header('Location: http://'.$_SERVER['SERVER_NAME'].$req.(isset($_GET['filter']) ? '?filter' : ''));
}
// }}}

// initial page {{{
else {
    page_header();
    display_cache();
    page_footer();
}
// }}}
